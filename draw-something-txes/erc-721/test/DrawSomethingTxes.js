// SPDX-License-Identifier: GPL-3.0-or-later

const { expect } = require("chai");

describe("DrawSomethingTxes contract", function () {

  let DS;
  let ds;
  let owner;
  let acct1;
  let acct2;
  let accts;
 
  beforeEach(async function () {
    DS = await ethers.getContractFactory("DrawSomethingTxes");
    [owner, acct1, acct2, ...accts] = await ethers.getSigners();
 
    ds = await DS.deploy();
  });

  it("Deployment should set correct initial contract state", async function () {
    expect(await ds.owner()).to.equal(owner.address);
    expect(await ds.name()).to.equal("draw-something (txes)");
    expect(await ds.symbol()).to.equal("DSTX");
    expect(await ds.totalSupply()).to.equal(0);
    expect(await ds.balanceOf(acct1.address)).to.equal(0);
    expect(await ds.MAX_TOKENS()).to.equal(24);
    expect(await ds.baseURI()).to.equal("");
  });

  it("Non-Owner should not be able to mint", async function () {
    await expect(ds.connect(acct1).mintToken())
      .to.be.revertedWith("Ownable: caller is not the owner");
    expect(await ds.balanceOf(acct1.address)).to.equal(0);
  });

  it("Owner should be able to mint", async function () {
    expect(await ds.balanceOf(owner.address)).to.equal(0);
    await ds.mintToken();
    expect(await ds.totalSupply()).to.equal(1);
    expect(await ds.balanceOf(owner.address)).to.equal(1);
  });

  it("Owner should not be able to mint > MAX_TOKENS", async function () {
    let MAX_TOKENS = await ds.MAX_TOKENS();
    while ((await ds.totalSupply()).lt(MAX_TOKENS)) {
      await ds.mintToken();
      const count = await ds.totalSupply();
      expect(await ds.ownerOf(count)).to.equal(owner.address);
      expect(await ds.totalSupply()).to.equal(count);
      expect(await ds.balanceOf(owner.address)).to.equal(count);
    }

    expect(await ds.totalSupply()).to.equal(24);
    expect(await ds.balanceOf(owner.address)).to.equal(24);
    
    await expect(ds.mintToken())
      .to.be.revertedWith("DSTx: all tokens minted");
  });

  it("Token owners should be able to transfer", async function () {
    let MAX_TOKENS = await ds.MAX_TOKENS();
    while ((await ds.totalSupply()).lt(MAX_TOKENS)) {
      await ds.mintToken();
    }
    expect(await ds.balanceOf(owner.address)).to.equal(24);

    await ds.transferFrom(owner.address, acct1.address, 1);
    expect(await ds.ownerOf(1)).to.equal(acct1.address);
    expect(await ds.totalSupply()).to.equal(24);
    expect(await ds.balanceOf(owner.address)).to.equal(23);
    expect(await ds.balanceOf(acct1.address)).to.equal(1);

    await ds.connect(acct1).transferFrom(acct1.address, acct2.address, 1);
    expect(await ds.ownerOf(1)).to.equal(acct2.address);
    expect(await ds.totalSupply()).to.equal(24);
    expect(await ds.balanceOf(owner.address)).to.equal(23);
    expect(await ds.balanceOf(acct1.address)).to.equal(0);
    expect(await ds.balanceOf(acct2.address)).to.equal(1);
  });

  it("non-token-owners should be able to transfer", async function () {
    let MAX_TOKENS = await ds.MAX_TOKENS();
    while ((await ds.totalSupply()).lt(MAX_TOKENS)) {
      await ds.mintToken();
    }
    expect(await ds.balanceOf(owner.address)).to.equal(24);
    await ds.transferFrom(owner.address, acct1.address, 1);
    await ds.connect(acct1).transferFrom(acct1.address, acct2.address, 1);

    await expect(
      ds.transferFrom(owner.address, acct1.address, 1)
    ).to.be.revertedWith("ERC721: transfer caller is not owner nor approved");
    await expect(
      ds.connect(acct1).transferFrom(acct1.address, acct2.address, 1)
    ).to.be.revertedWith("ERC721: transfer caller is not owner nor approved");
    await expect(
      ds.connect(acct2).transferFrom(acct2.address, acct1.address, 2)
    ).to.be.revertedWith("ERC721: transfer caller is not owner nor approved");
  });

  it("URLs should be handled correctly", async function () {
    let MAX_TOKENS = await ds.MAX_TOKENS();
    while ((await ds.totalSupply()).lt(MAX_TOKENS)) {
      await ds.mintToken();
    }
    expect(await ds.balanceOf(owner.address)).to.equal(24);

    await expect(ds.tokenURI(0))
      .to.be.revertedWith("ERC721Metadata: URI query for nonexistent token");
    await expect(ds.tokenURI(25))
      .to.be.revertedWith("ERC721Metadata: URI query for nonexistent token");

    expect(await ds.tokenURI(1)).to.equal("");

    await ds.setBaseURI("---");
    expect(await ds.tokenURI(1)).to.equal("---1");
    
    await expect(ds.connect(acct1).setBaseURI("+++++++++"))
      .to.be.revertedWith("Ownable: caller is not the owner");
  });

  it("Token minting events should be available", async function () {
    let MAX_TOKENS = await ds.MAX_TOKENS();
    while ((await ds.totalSupply()).lt(MAX_TOKENS)) {
      await ds.mintToken();
    }
    expect(await ds.balanceOf(owner.address)).to.equal(24);

    let filter = ds.filters.Transfer(
      "0x0000000000000000000000000000000000000000",
      owner.address,
      null
    );
    let events = await ds.queryFilter(filter);
    expect(events.length).to.equal(24);
  });
  
});
