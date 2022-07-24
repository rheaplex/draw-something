draw-something (txes)
=====================

To deploy:

1. Deploy the ERC721 contract, create the tokens, and get their minting hashes. See erc721/README.md for details.
2. Make the build dir: `mkdir -p build` .
3. Generate the drawings: `./generate-drawings` .
4. Generate a preview montage: `./montage` .
5. Preview it: `display montage.png` .
6. Generate other image formats: `./generate-other-formats` .
7. TODO.
