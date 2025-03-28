;; title: LogisticsChain
;; version: 1.0.0
;; summary: Supply chain optimization platform
;; description: Smart contract for managing supply chain logistics with real-time tracking, quality assurance, and automated settlements

;; traits
(define-trait subscription-trait
  (
    (get-subscription-status (principal) (response bool uint))
    (subscribe (uint) (response bool uint))
    (unsubscribe () (response bool uint))
  )
)

;; token definitions
;; None needed for basic functionality

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-not-subscriber (err u104))
(define-constant err-payment-failed (err u105))

;; data vars
(define-data-var next-shipment-id uint u1)
(define-data-var next-company-id uint u1)
(define-data-var subscription-fee uint u100) ;; in STX

;; data maps
(define-map companies
  { company-id: uint }
  {
    name: (string-ascii 100),
    owner: principal,
    subscription-active: bool,
    subscription-expiry: uint
  }
)

(define-map company-principals
  { principal: principal }
  { company-id: uint }
)

(define-map shipments
  { shipment-id: uint }
  {
    origin: uint,  ;; company-id
    destination: uint,  ;; company-id
    product: (string-ascii 100),
    quantity: uint,
    created-at: uint,
    status: (string-ascii 20),  ;; "created", "in-transit", "delivered", "rejected"
    quality-score: (optional uint),
    last-updated: uint,
    settlement-complete: bool
  }
)

(define-map shipment-tracking
  { shipment-id: uint, timestamp: uint }
  {
    location: (string-ascii 100),
    temperature: int,
    humidity: uint,
    notes: (string-ascii 255)
  }
)

(define-map quality-requirements
  { product: (string-ascii 100) }
  {
    min-temperature: int,
    max-temperature: int,
    min-humidity: uint,
    max-humidity: uint
  }
)

;; public functions
(define-public (register-company (name (string-ascii 100)))
  (let
    (
      (company-id (var-get next-company-id))
    )
    (asserts! (is-none (map-get? company-principals { principal: tx-sender })) err-already-exists)
    
    (map-set companies
      { company-id: company-id }
      {
        name: name,
        owner: tx-sender,
        subscription-active: false,
        subscription-expiry: u0
      }
    )
    
    (map-set company-principals
      { principal: tx-sender }
      { company-id: company-id }
    )
    
    (var-set next-company-id (+ company-id u1))
    (ok company-id)
  )
)

(define-public (subscribe (duration uint))
  (let
    (
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (company-id (get company-id company-data))
      (fee (* (var-get subscription-fee) duration))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
      (company (unwrap! (map-get? companies { company-id: company-id }) err-not-found))
      (new-expiry (if (> (get subscription-expiry company) current-time)
                     (+ (get subscription-expiry company) (* duration u2592000)) ;; duration in months (30 days in seconds)
                     (+ current-time (* duration u2592000))))
    )
    
    ;; Transfer subscription fee to contract
    (unwrap! (stx-transfer? fee tx-sender (as-contract tx-sender)) err-payment-failed)
    
    ;; Update subscription status
    (map-set companies
      { company-id: company-id }
      (merge company {
        subscription-active: true,
        subscription-expiry: new-expiry
      })
    )
    
    (ok true)
  )
)

(define-public (create-shipment (destination-id uint) (product (string-ascii 100)) (quantity uint))
  (let
    (
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (origin-id (get company-id company-data))
      (shipment-id (var-get next-shipment-id))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))

    )
    
    ;; Check if company has active subscription
    (asserts! (is-subscription-active origin-id) err-not-subscriber)
    (asserts! (is-subscription-active destination-id) err-not-subscriber)
    
    (map-set shipments
      { shipment-id: shipment-id }
      {
        origin: origin-id,
        destination: destination-id,
        product: product,
        quantity: quantity,
        created-at: current-time,
        status: "created",
        quality-score: none,
        last-updated: current-time,
        settlement-complete: false
      }
    )
    
    (var-set next-shipment-id (+ shipment-id u1))
    (ok shipment-id)
  )
)

(define-public (update-shipment-status (shipment-id uint) (new-status (string-ascii 20)))
  (let
    (
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (company-id (get company-id company-data))
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) err-not-found))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    
    ;; Check if company has active subscription
    (asserts! (is-subscription-active company-id) err-not-subscriber)
    
    ;; Check if company is authorized to update this shipment
    (asserts! (or (is-eq (get origin shipment) company-id) (is-eq (get destination shipment) company-id)) err-not-authorized)
    
    ;; Check if new status is valid
    (asserts! (or (is-eq new-status "in-transit") (is-eq new-status "delivered") (is-eq new-status "rejected")) err-invalid-status)
    
    ;; Update shipment status
    (map-set shipments
      { shipment-id: shipment-id }
      (merge shipment {
        status: new-status,
        last-updated: current-time
      })
    )
    
    ;; If delivered or rejected, trigger settlement
    (if (or (is-eq new-status "delivered") (is-eq new-status "rejected"))
        (settle-shipment shipment-id)
        (ok true))
  )
)

(define-public (add-tracking-data (shipment-id uint) (location (string-ascii 100)) (temperature int) (humidity uint) (notes (string-ascii 255)))
  (let
    (
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (company-id (get company-id company-data))
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) err-not-found))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    
    ;; Check if company has active subscription
    (asserts! (is-subscription-active company-id) err-not-subscriber)
    
    ;; Check if company is authorized to update this shipment
    (asserts! (or (is-eq (get origin shipment) company-id) (is-eq (get destination shipment) company-id)) err-not-authorized)
    
    ;; Add tracking data
    (map-set shipment-tracking
      { shipment-id: shipment-id, timestamp: current-time }
      {
        location: location,
        temperature: temperature,
        humidity: humidity,
        notes: notes
      }
    )
    
    (ok true)
  )
)

(define-public (set-quality-requirements (product (string-ascii 100)) (min-temp int) (max-temp int) (min-humidity uint) (max-humidity uint))
  (let
    (
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (company-id (get company-id company-data))
    )
    
    ;; Check if company has active subscription
    (asserts! (is-subscription-active company-id) err-not-subscriber)
    
    (map-set quality-requirements
      { product: product }
      {
        min-temperature: min-temp,
        max-temperature: max-temp,
        min-humidity: min-humidity,
        max-humidity: max-humidity
      }
    )
    
    (ok true)
  )
)

(define-public (assess-quality (shipment-id uint) (quality-score uint))
  (let
    (
      (company-data (unwrap! (get-company-by-principal tx-sender) err-not-found))
      (company-id (get company-id company-data))
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) err-not-found))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    
    ;; Check if company has active subscription
    (asserts! (is-subscription-active company-id) err-not-subscriber)
    
    ;; Check if company is the destination (receiver)
    (asserts! (is-eq (get destination shipment) company-id) err-not-authorized)
    
    ;; Check if quality score is valid (0-100)
    (asserts! (<= quality-score u100) (err u106))
    
    ;; Update shipment with quality score
    (map-set shipments
      { shipment-id: shipment-id }
      (merge shipment {
        quality-score: (some quality-score),
        last-updated: current-time
      })
    )
    
    (ok true)
  )
)

(define-public (settle-shipment (shipment-id uint))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) err-not-found))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    
    ;; Check if shipment is delivered or rejected
    (asserts! (or (is-eq (get status shipment) "delivered") (is-eq (get status shipment) "rejected")) err-invalid-status)
    
    ;; Check if settlement is not already complete
    (asserts! (not (get settlement-complete shipment)) (err u107))
    
    ;; Mark settlement as complete
    (map-set shipments
      { shipment-id: shipment-id }
      (merge shipment {
        settlement-complete: true,
        last-updated: current-time
      })
    )
    
    
    (ok true)
  )
)

(define-public (update-subscription-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (var-set subscription-fee new-fee)
    (ok true)
  )
)

;; read only functions
(define-read-only (get-shipment (shipment-id uint))
  (map-get? shipments { shipment-id: shipment-id })
)

(define-read-only (get-company (company-id uint))
  (map-get? companies { company-id: company-id })
)

(define-read-only (get-company-by-principal (principal principal))
  (map-get? company-principals { principal: principal })
)

(define-read-only (get-tracking-history (shipment-id uint) (timestamp uint))
  (map-get? shipment-tracking { shipment-id: shipment-id, timestamp: timestamp })
)

(define-read-only (get-quality-requirements (product (string-ascii 100)))
  (map-get? quality-requirements { product: product })
)

(define-read-only (get-subscription-fee)
  (var-get subscription-fee)
)

(define-read-only (is-subscription-active (company-id uint))
  (let
    (
      (company (unwrap! (map-get? companies { company-id: company-id }) false))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))

    )
    (and (get subscription-active company) (> (get subscription-expiry company) current-time))
  )
)

;; private functions
(define-private (is-authorized-for-shipment (shipment-id uint) (company-id uint))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) false))
    )
    (or (is-eq (get origin shipment) company-id) (is-eq (get destination shipment) company-id))
  )
)
