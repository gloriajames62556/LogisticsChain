# LogisticsChain

A blockchain-based supply chain optimization platform built on Stacks using Clarity smart contracts.

## Overview

LogisticsChain provides a decentralized solution for supply chain management with features including real-time tracking, quality assurance, and automated settlements. The platform enables companies to gain complete visibility into their supply chains while ensuring data integrity and trust between parties.

## Features

- **Real-time Tracking**: Monitor shipments throughout the entire logistics process
- **Quality Assurance**: Define and enforce quality requirements for products  
- **Automated Settlements**: Trigger payments based on delivery status and quality metrics
- **Subscription Model**: Companies subscribe to access platform features
- **Immutable Record-Keeping**: All supply chain events are permanently recorded on the blockchain

## Smart Contract Architecture

The LogisticsChain platform is built on a Clarity smart contract that manages:

- Company registration and subscription management
- Shipment creation and status updates
- Quality requirements and assessments 
- Tracking data collection
- Automated settlement processes

## Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) - Clarity development environment
- [Node.js](https://nodejs.org/) - For running tests

### Installation

1. Clone the repository:
```bash
git clone https://github.com/gloriajames62556/LogisticsChain.git
cd LogisticsChain
```

2. Install dependencies:
```bash
npm install
```

### Testing

The project uses Vitest with Clarinet integration for testing:

```bash
npm test
```

## Contract Functions

### Company Management

- `register-company`: Register a new company on the platform
- `subscribe`: Purchase a subscription for a specified duration
- `is-subscription-active`: Check if a company has an active subscription

### Shipment Management 

- `create-shipment`: Create a new shipment between two companies
- `update-shipment-status`: Update the status of a shipment (in-transit, delivered, rejected)
- `add-tracking-data`: Add location, temperature, humidity, and other tracking data
- `assess-quality`: Evaluate the quality of delivered goods
- `settle-shipment`: Complete the transaction process for a shipment

### Administrative Functions

- `update-subscription-fee`: Update the platform subscription fee (owner only)

### Read-Only Functions

- `get-shipment`: Retrieve shipment details
- `get-company`: Get company information
- `get-tracking-history`: View tracking data for a shipment
- `get-quality-requirements`: Check quality requirements for a product
- `get-subscription-fee`: View current subscription fee

## Use Cases

- **Food Supply Chain**: Track temperature-sensitive food products from farm to table
- **Pharmaceutical Distribution**: Ensure medication quality through verified transport conditions
- **Manufacturing**: Monitor component quality and delivery for just-in-time production
- **Retail**: Verify authenticity and condition of high-value goods

## Development

### Project Structure

```
LogisticsChain/
├── contracts/
│   └── LogisticsChain.clar    # Main smart contract
├── tests/                     # Test directory
├── vitest.config.js           # Vitest configuration
└── README.md                  # Project documentation
```

### Development Workflow

1. Make changes to the Clarity contract
2. Write tests to verify functionality
3. Run tests with Vitest
4. Deploy to testnet for integration testing
5. Deploy to mainnet for production use

## License

[MIT License](LICENSE)

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Contact

Project Link: [https://github.com/gloriajames62556/LogisticsChain](https://github.com/gloriajames62556/LogisticsChain)
