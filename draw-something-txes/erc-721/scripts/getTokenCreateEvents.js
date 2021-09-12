// SPDX-License-Identifier: GPL-3.0-or-later

const fs = require('fs');

async function main() {
  const { abi } = JSON.parse(
    fs.readFileSync(
      `${__dirname}/../artifacts/contracts/DrawSomethingTxes.sol/DrawSomethingTxes.json`
    ).toString());
  const { address } = JSON.parse(fs.readFileSync(
      `${__dirname}/../addresses/DrawSomethingTxes-${network.name}.json`
  ).toString());
 
  const [ signer ] = await ethers.getSigners();
  
  const ds = new ethers.Contract(address, abi, signer);

  const MAX_TOKENS = await ds.MAX_TOKENS();
  
  let count = await ds.totalSupply();
  
  let filter = ds.filters.Transfer(
      "0x0000000000000000000000000000000000000000",
      null,
      null
    );
  let events = await ds.queryFilter(filter);
  events.forEach((event, index) => {
    if(event.args.tokenId.toNumber() != (index+ 1)) {
      console.error("TXES OUT OF ORDER");
      process.exit(1);
    }
    let id = event.transactionHash.substr(2);
    if(id.length != 64) {
      console.error("TX HASH ID WRONG LENGTH");
      process.exit(1);
    }
    console.log(id);
  });

  /*let mintHashes = events.map(
    event => ({
      tokenId: event.args.tokenId.toNumber(),
      creationTxHash: event.transactionHash,
    })
  );

  console.log(JSON.stringify(mintHashes, null, 2));*/
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });
