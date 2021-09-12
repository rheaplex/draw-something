require("@nomiclabs/hardhat-waffle");

/**
 * @type import('hardhat/config').HardhatUserConfig
 */
module.exports = {
  solidity: "0.8.6",
  networks: {
    ganache: {
      url: `http://127.0.0.1:5485`,
      accounts: [
        "0xa1b1e970965814cc0915fb84574e8d3b6ca1b0ed397e6725948bd8156a9559af",
        "0x95232331e400484d081d14f27942f2d2176d3b897daf17ec5e5554272bd3722f",
        "0xf3a37e443cf3f14ed20a44ac287997011d5cdbbfea6073d3cd35e6fb726d9f05",
        "0x5b9d229b31c54c05d888bb869b56ff7630002c36f11d6e078c6abbd65436b50c",
      ],
    },
  },
};
