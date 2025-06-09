# Functional Webhook

This project is a functional webhook receiver implemented in Haskell. It validates and processes incoming transaction events with safety checks such as duplicate detection, field validation, and request authentication via a shared token.

## Features

* Listens for webhook events on the `/webhook` endpoint.
* Validates a custom token provided in the `X-Webhook-Token` header.
* Parses incoming JSON payloads into strongly-typed Haskell records.
* Cancels transactions with missing or invalid fields.
* Ignores duplicate transactions.
* Confirms successful transactions by sending a follow-up HTTP request to an external confirmation endpoint.
* Cancels transactions with amount equal to zero.
* Logs malformed payloads and attempts to cancel them if they contain a `transaction_id`.

## Webhook Payload Format

Expected JSON format:

```json
{
  "event": "payment_success",
  "transaction_id": "abc123",
  "amount": "49.90",
  "currency": "BRL",
  "timestamp": "2025-05-11T16:00:00Z"
}
```

> **Note:** All fields must be non-empty strings. The `amount` must be passed as a **string**.

## Requirements

* [GHC](https://www.haskell.org/ghc/) (>= 9.2 recommended)
* [Cabal](https://www.haskell.org/cabal/) (>= 3.0)

## Installation

1. Clone the repository:

```bash
git clone https://github.com/PedroBittarBarao/Functional-Webhook.git
cd Functional-Webhook
```

2. Build the project:

```bash
cabal build
```

## Running

To start the webhook server:

```bash
cabal run Functional-Webhook
```

By default, it listens on port `5001`.

## Environment

This webhook expects requests to contain the header:

```
X-Webhook-Token: meu-token-secreto
```

Modify the `expectedToken` variable in `RequestHandler.hs` if you want to change the token value.

## Behavior Summary

| Case                         | Action                                  |
| ---------------------------- | --------------------------------------- |
| Invalid or missing token     | Responds with 403                       |
| Missing required fields      | Cancels transaction, responds with 200  |
| amount == "0.00"             | Cancels transaction, responds with 422  |
| Duplicate transaction\_id    | Rejects with 400                        |
| Valid payload                | Confirms transaction, responds with 200 |
| Invalid JSON but contains ID | Cancels transaction, responds with 400  |
| Completely malformed payload | Responds with 400                       |

## License

This project is licensed under the MIT License.
