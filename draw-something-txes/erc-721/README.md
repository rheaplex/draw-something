draw-something (txes)
=====================

"draw-something, but on the blockchain"

You'll need node, npm, npx and hardhat installed.


Testing
=======

Running Tests
-------------

Run:

``` shell
npx run hardhat test
```

Local Testing Of Scripts
------------------------

Install and run ganache-cli in one terminal window.
Note that we use a non-standard port and a specific mnemonic:

``` shell
npm install -g ganache-cli
ganache-cli --port 5485 --mnemonic 'charge control never below detail kite antenna plunge limb city apart artwork'
```

Run the scripts in another teminal window:

``` shell
npx hardhat run scripts/deploy.js --network ganache
npx hardhat run scripts/createTokens.js --network ganache
npx hardhat run scripts/getTokenCreateEvents.js --network ganache > ../tx-hashes
```
