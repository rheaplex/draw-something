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
  
  const ds_rw = new ethers.Contract(address, abi, signer);

  const MAX_TOKENS = await ds_rw.MAX_TOKENS();
  
  let count = await ds_rw.totalSupply();
  console.log(`${count} of ${MAX_TOKENS} minted on ${network.name}.`);
  while (count.lt(MAX_TOKENS)) {
    const tx = await ds_rw.mintToken();
    // We do not want to use the hash or mint the next token
    // until this one is successfully on the chain.
    const receipt = await tx.wait();
    // 0 is error, a la UNIX
    if (receipt.status === 0) {
      throw new Error(
        `Minting transaction for token #${count} failed. Sadface.`
      );
    }
    count = await ds_rw.totalSupply();
    console.log(`${count}: ${tx.hash}`);
  }
  console.log("All tokens minted.");
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });
