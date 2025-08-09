;; Digital Pickup Sports Organizer
;; A decentralized platform for organizing casual sports games
;; Handles player registration, skill matching, and court booking

;; ==========================================================================
;; CONSTANTS & ERROR CODES
;; ==========================================================================

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-GAME-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-REGISTERED (err u102))
(define-constant ERR-GAME-FULL (err u103))
(define-constant ERR-GAME-STARTED (err u104))
(define-constant ERR-INVALID-SKILL-LEVEL (err u105))
(define-constant ERR-COURT-NOT-AVAILABLE (err u106))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u107))
(define-constant ERR-GAME-NOT-STARTED (err u108))
(define-constant ERR-PLAYER-NOT-REGISTERED (err u109))
(define-constant ERR-INVALID-TIME (err u110))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-PLAYERS-PER-GAME u20)
(define-constant MIN-PLAYERS-PER-GAME u4)
(define-constant COURT-BOOKING-FEE u1000000) ;; 1 STX in microSTX

;; Sports types
(define-constant SPORT-BASKETBALL u1)
(define-constant SPORT-SOCCER u2)
(define-constant SPORT-TENNIS u3)
(define-constant SPORT-VOLLEYBALL u4)

;; Skill levels (1-5 scale)
(define-constant SKILL-BEGINNER u1)
(define-constant SKILL-INTERMEDIATE u2)
(define-constant SKILL-ADVANCED u3)
(define-constant SKILL-EXPERT u4)
(define-constant SKILL-PROFESSIONAL u5)

;; ==========================================================================
;; DATA STRUCTURES
;; ==========================================================================

;; Player profile data
(define-map players
  principal
  {
    username: (string-ascii 50),
    skill-levels: {
      basketball: uint,
      soccer: uint,
      tennis: uint,
      volleyball: uint
    },
    games-played: uint,
    rating: uint,
    created-at: uint
  }
)

;; Game data structure
(define-map games
  uint ;; game-id
  {
    organizer: principal,
    sport-type: uint,
    court-id: uint,
    max-players: uint,
    current-players: uint,
    skill-level-range: {min: uint, max: uint},
    start-time: uint,
    duration: uint, ;; in blocks
    location: (string-ascii 100),
    description: (string-ascii 500),
    status: uint, ;; 0=open, 1=full, 2=started, 3=completed, 4=cancelled
    created-at: uint,
    booking-fee-paid: bool
  }
)

;; Game participants
(define-map game-participants
  {game-id: uint, player: principal}
  {
    joined-at: uint,
    skill-level: uint,
    confirmed: bool
  }
)

;; Court booking system
(define-map courts
  uint ;; court-id
  {
    name: (string-ascii 100),
    location: (string-ascii 200),
    sport-types: (list 10 uint),
    hourly-rate: uint,
    available: bool
  }
)

;; Court bookings
(define-map court-bookings
  {court-id: uint, time-slot: uint}
  {
    game-id: uint,
    booked-by: principal,
    duration: uint,
    fee-paid: uint
  }
)

;; ==========================================================================
;; DATA VARIABLES
;; ==========================================================================

(define-data-var next-game-id uint u1)
(define-data-var next-court-id uint u1)
(define-data-var platform-fee-rate uint u50) ;; 5% in basis points

;; ==========================================================================
;; PLAYER MANAGEMENT FUNCTIONS
;; ==========================================================================

;; Register a new player
(define-public (register-player
    (username (string-ascii 50))
    (basketball-skill uint)
    (soccer-skill uint)
    (tennis-skill uint)
    (volleyball-skill uint))
  (let ((player tx-sender))
    (asserts! (is-none (map-get? players player)) ERR-ALREADY-REGISTERED)
    (asserts! (and (<= basketball-skill SKILL-PROFESSIONAL) (>= basketball-skill SKILL-BEGINNER)) ERR-INVALID-SKILL-LEVEL)
    (asserts! (and (<= soccer-skill SKILL-PROFESSIONAL) (>= soccer-skill SKILL-BEGINNER)) ERR-INVALID-SKILL-LEVEL)
    (asserts! (and (<= tennis-skill SKILL-PROFESSIONAL) (>= tennis-skill SKILL-BEGINNER)) ERR-INVALID-SKILL-LEVEL)
    (asserts! (and (<= volleyball-skill SKILL-PROFESSIONAL) (>= volleyball-skill SKILL-BEGINNER)) ERR-INVALID-SKILL-LEVEL)

    (map-set players player {
      username: username,
      skill-levels: {
        basketball: basketball-skill,
        soccer: soccer-skill,
        tennis: tennis-skill,
        volleyball: volleyball-skill
      },
      games-played: u0,
      rating: u3, ;; Default to intermediate
      created-at: stacks-block-height
    })
    (ok true)))

;; Update player skill levels
(define-public (update-skill-levels
    (basketball-skill uint)
    (soccer-skill uint)
    (tennis-skill uint)
    (volleyball-skill uint))
  (let ((player tx-sender)
        (existing-profile (unwrap! (map-get? players player) ERR-PLAYER-NOT-REGISTERED)))

    (asserts! (and (<= basketball-skill SKILL-PROFESSIONAL) (>= basketball-skill SKILL-BEGINNER)) ERR-INVALID-SKILL-LEVEL)
    (asserts! (and (<= soccer-skill SKILL-PROFESSIONAL) (>= soccer-skill SKILL-BEGINNER)) ERR-INVALID-SKILL-LEVEL)
    (asserts! (and (<= tennis-skill SKILL-PROFESSIONAL) (>= tennis-skill SKILL-BEGINNER)) ERR-INVALID-SKILL-LEVEL)
    (asserts! (and (<= volleyball-skill SKILL-PROFESSIONAL) (>= volleyball-skill SKILL-BEGINNER)) ERR-INVALID-SKILL-LEVEL)

    (map-set players player (merge existing-profile {
      skill-levels: {
        basketball: basketball-skill,
        soccer: soccer-skill,
        tennis: tennis-skill,
        volleyball: volleyball-skill
      }
    }))
    (ok true)))

;; ==========================================================================
;; COURT MANAGEMENT FUNCTIONS
;; ==========================================================================

;; Add a new court (only contract owner)
(define-public (add-court
    (name (string-ascii 100))
    (location (string-ascii 200))
    (sport-types (list 10 uint))
    (hourly-rate uint))
  (let ((court-id (var-get next-court-id)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)

    (map-set courts court-id {
      name: name,
      location: location,
      sport-types: sport-types,
      hourly-rate: hourly-rate,
      available: true
    })

    (var-set next-court-id (+ court-id u1))
    (ok court-id)))

;; Check court availability for a specific time slot
(define-read-only (is-court-available (court-id uint) (time-slot uint) (duration uint))
  (let ((court (unwrap! (map-get? courts court-id) false)))
    (if (get available court)
      (is-none (map-get? court-bookings {court-id: court-id, time-slot: time-slot}))
      false)))

;; ==========================================================================
;; GAME MANAGEMENT FUNCTIONS
;; ==========================================================================

;; Create a new game
(define-public (create-game
    (sport-type uint)
    (court-id uint)
    (max-players uint)
    (min-skill uint)
    (max-skill uint)
    (start-time uint)
    (duration uint)
    (location (string-ascii 100))
    (description (string-ascii 500)))
  (let ((game-id (var-get next-game-id))
        (organizer tx-sender)
        (court (unwrap! (map-get? courts court-id) ERR-COURT-NOT-AVAILABLE)))

    ;; Validations
    (asserts! (is-some (map-get? players organizer)) ERR-PLAYER-NOT-REGISTERED)
    (asserts! (and (<= max-players MAX-PLAYERS-PER-GAME) (>= max-players MIN-PLAYERS-PER-GAME)) ERR-INVALID-SKILL-LEVEL)
    (asserts! (and (<= min-skill SKILL-PROFESSIONAL) (>= min-skill SKILL-BEGINNER)) ERR-INVALID-SKILL-LEVEL)
    (asserts! (and (<= max-skill SKILL-PROFESSIONAL) (>= max-skill min-skill)) ERR-INVALID-SKILL-LEVEL)
    (asserts! (> start-time stacks-block-height) ERR-INVALID-TIME)
    (asserts! (is-court-available court-id start-time duration) ERR-COURT-NOT-AVAILABLE)

    ;; Create the game
    (map-set games game-id {
      organizer: organizer,
      sport-type: sport-type,
      court-id: court-id,
      max-players: max-players,
      current-players: u1, ;; Organizer is automatically registered
      skill-level-range: {min: min-skill, max: max-skill},
      start-time: start-time,
      duration: duration,
      location: location,
      description: description,
      status: u0, ;; Open
      created-at: stacks-block-height,
      booking-fee-paid: false
    })

    ;; Register organizer as first participant
    (map-set game-participants {game-id: game-id, player: organizer} {
      joined-at: stacks-block-height,
      skill-level: (get-player-skill-for-sport organizer sport-type),
      confirmed: true
    })

    (var-set next-game-id (+ game-id u1))
    (ok game-id)))

;; Join a game
(define-public (join-game (game-id uint))
  (let ((player tx-sender)
        (game (unwrap! (map-get? games game-id) ERR-GAME-NOT-FOUND))
        (player-profile (unwrap! (map-get? players player) ERR-PLAYER-NOT-REGISTERED))
        (player-skill (get-player-skill-for-sport player (get sport-type game))))

    ;; Validations
    (asserts! (is-none (map-get? game-participants {game-id: game-id, player: player})) ERR-ALREADY-REGISTERED)
    (asserts! (< (get current-players game) (get max-players game)) ERR-GAME-FULL)
    (asserts! (is-eq (get status game) u0) ERR-GAME-STARTED)
    (asserts! (and
      (>= player-skill (get min (get skill-level-range game)))
      (<= player-skill (get max (get skill-level-range game)))) ERR-INVALID-SKILL-LEVEL)

    ;; Add player to game
    (map-set game-participants {game-id: game-id, player: player} {
      joined-at: stacks-block-height,
      skill-level: player-skill,
      confirmed: true
    })

    ;; Update game player count
    (map-set games game-id (merge game {
      current-players: (+ (get current-players game) u1),
      status: (if (is-eq (+ (get current-players game) u1) (get max-players game)) u1 u0)
    }))

    (ok true)))

;; Leave a game (before it starts)
(define-public (leave-game (game-id uint))
  (let ((player tx-sender)
        (game (unwrap! (map-get? games game-id) ERR-GAME-NOT-FOUND))
        (participation (unwrap! (map-get? game-participants {game-id: game-id, player: player}) ERR-PLAYER-NOT-REGISTERED)))

    ;; Validations
    (asserts! (is-eq (get status game) u0) ERR-GAME-STARTED)
    (asserts! (not (is-eq player (get organizer game))) ERR-NOT-AUTHORIZED) ;; Organizer can't leave

    ;; Remove player from game
    (map-delete game-participants {game-id: game-id, player: player})

    ;; Update game player count
    (map-set games game-id (merge game {
      current-players: (- (get current-players game) u1),
      status: u0 ;; Reset to open
    }))

    (ok true)))

;; Book court and pay fees
(define-public (book-court-for-game (game-id uint))
  (let ((game (unwrap! (map-get? games game-id) ERR-GAME-NOT-FOUND))
        (court (unwrap! (map-get? courts (get court-id game)) ERR-COURT-NOT-AVAILABLE))
        (total-fee (+ COURT-BOOKING-FEE (* (get hourly-rate court) (get duration game)))))

    ;; Only organizer can book court
    (asserts! (is-eq tx-sender (get organizer game)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get booking-fee-paid game)) ERR-ALREADY-REGISTERED)
    (asserts! (is-court-available (get court-id game) (get start-time game) (get duration game)) ERR-COURT-NOT-AVAILABLE)

    ;; Transfer payment
    (try! (stx-transfer? total-fee tx-sender (as-contract tx-sender)))

    ;; Book the court
    (map-set court-bookings
      {court-id: (get court-id game), time-slot: (get start-time game)}
      {
        game-id: game-id,
        booked-by: tx-sender,
        duration: (get duration game),
        fee-paid: total-fee
      })

    ;; Update game
    (map-set games game-id (merge game {booking-fee-paid: true}))

    (ok true)))

;; Start a game (organizer only)
(define-public (start-game (game-id uint))
  (let ((game (unwrap! (map-get? games game-id) ERR-GAME-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer game)) ERR-NOT-AUTHORIZED)
    (asserts! (>= (get current-players game) MIN-PLAYERS-PER-GAME) ERR-INVALID-SKILL-LEVEL)
    (asserts! (is-eq (get status game) u0) ERR-GAME-STARTED)
    (asserts! (get booking-fee-paid game) ERR-INSUFFICIENT-PAYMENT)

    (map-set games game-id (merge game {status: u2})) ;; Started
    (ok true)))

;; Complete a game and update player ratings
(define-public (complete-game (game-id uint))
  (let ((game (unwrap! (map-get? games game-id) ERR-GAME-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer game)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status game) u2) ERR-GAME-NOT-STARTED)

    ;; Update game status
    (map-set games game-id (merge game {status: u3})) ;; Completed

    ;; Update player stats (simplified - in production would be more sophisticated)
    (try! (update-player-stats-for-game game-id))

    (ok true)))

;; ==========================================================================
;; HELPER FUNCTIONS
;; ==========================================================================

;; Get player skill level for specific sport
(define-read-only (get-player-skill-for-sport (player principal) (sport-type uint))
  (let ((profile (unwrap! (map-get? players player) u0))
        (skills (get skill-levels profile)))
    (if (is-eq sport-type SPORT-BASKETBALL)
      (get basketball skills)
      (if (is-eq sport-type SPORT-SOCCER)
        (get soccer skills)
        (if (is-eq sport-type SPORT-TENNIS)
          (get tennis skills)
          (if (is-eq sport-type SPORT-VOLLEYBALL)
            (get volleyball skills)
            u0))))))

;; Update player statistics after game completion
(define-private (update-player-stats-for-game (game-id uint))
  (let ((game (unwrap! (map-get? games game-id) ERR-GAME-NOT-FOUND)))
    ;; This would iterate through all participants and update their stats
    ;; Simplified implementation for demo
    (ok true)))

;; ==========================================================================
;; READ-ONLY FUNCTIONS
;; ==========================================================================

;; Get player profile
(define-read-only (get-player (player principal))
  (map-get? players player))

;; Get game details
(define-read-only (get-game (game-id uint))
  (map-get? games game-id))

;; Get game participants
(define-read-only (get-game-participation (game-id uint) (player principal))
  (map-get? game-participants {game-id: game-id, player: player}))

;; Get court details
(define-read-only (get-court (court-id uint))
  (map-get? courts court-id))

;; Get court booking
(define-read-only (get-court-booking (court-id uint) (time-slot uint))
  (map-get? court-bookings {court-id: court-id, time-slot: time-slot}))

;; Get games by skill level range
(define-read-only (get-games-in-skill-range (sport-type uint) (player-skill uint))
  ;; This would return a filtered list of games matching the player's skill level
  ;; Implementation would require iteration through games map
  (ok true))

;; Get available courts for sport type
(define-read-only (get-available-courts-for-sport (sport-type uint))
  ;; Returns courts that support the specified sport type
  (ok true))

;; ==========================================================================
;; ADMIN FUNCTIONS
;; ==========================================================================

;; Update platform fee rate (owner only)
(define-public (set-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set platform-fee-rate new-rate)
    (ok true)))

;; Withdraw platform fees (owner only)
(define-public (withdraw-fees (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (ok true)))
