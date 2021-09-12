pragma solidity ^0.8.0;

// SPDX-License-Identifier: GPL-3.0-or-later

import "@openzeppelin/contracts/access/Ownable.sol";
import "@openzeppelin/contracts/token/ERC721/ERC721.sol";
import "@openzeppelin/contracts/token/ERC721/extensions/ERC721Enumerable.sol";

contract DrawSomethingTxes is ERC721, ERC721Enumerable, Ownable {
    uint256 public constant MAX_TOKENS = 24;

    string public baseURI;
    
    constructor() ERC721("draw-something (txes)", "DSTX") {
    }

    function mintToken() external onlyOwner {
        // No token zero.
        // Save a member variable by using totalSupply() .
        uint256 nextID = totalSupply() + 1;
        // Less than or equal because no token zero.
        require(nextID <= MAX_TOKENS, "DSTx: all tokens minted");
        _safeMint(msg.sender, nextID);
    }

    function _baseURI() internal view virtual override returns (string memory) {
        return baseURI;
    }

    function setBaseURI(string memory uri) external onlyOwner {
        baseURI = uri;
    }

    function supportsInterface(bytes4 interfaceId)
        public
        view
        virtual
        override(ERC721, ERC721Enumerable)
        returns (bool)
    {
        return super.supportsInterface(interfaceId);
    }

    function _beforeTokenTransfer(
        address from,
        address to,
        uint256 tokenId
    ) internal virtual override(ERC721, ERC721Enumerable) {
        super._beforeTokenTransfer(from, to, tokenId);
    }
}
