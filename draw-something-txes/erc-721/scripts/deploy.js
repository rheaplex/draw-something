// SPDX-License-Identifier: GPL-3.0-or-later

const fs = require("fs");

async function main() {
  const [deployer] = await ethers.getSigners();

  console.log("Deploying contracts with the account:", deployer.address);
  console.log("Account balance:", (await deployer.getBalance()).toString());

  const DrawSomethingTxes = await ethers.getContractFactory(
    "DrawSomethingTxes"
  );
  const drawSomethingTxes = await DrawSomethingTxes.deploy();
  await drawSomethingTxes.deployed();

  console.log(
    `DrawSomethingTxes ${network.name} address: ${drawSomethingTxes.address}`
  );

  const addressesDir = `${__dirname}/../addresses`;
  
  if (!fs.existsSync(addressesDir)) {
    fs.mkdirSync(addressesDir);
  }

  fs.writeFileSync(
    `${addressesDir}/DrawSomethingTxes-${network.name}.json`,
    JSON.stringify({ address: drawSomethingTxes.address }, null, 2)
  );
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });
