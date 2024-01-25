{ mkDerivation, ansi-terminal, async, attoparsec, base, bytestring
, cassava, containers, data-default, deepseq, directory, extra
, filepath, hermes-json, HUnit, lib, lock-file, MemoTrie, optics
, random, relude, safe, stm, streamly-core, strict, strict-types
, terminal-size, text, time, transformers, typed-process, unix
, vector, word8
}:
mkDerivation {
  pname = "nix-output-monitor";
  version = "2.1.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal async attoparsec base bytestring cassava containers
    data-default deepseq directory extra filepath hermes-json lock-file
    MemoTrie optics relude safe stm streamly-core strict strict-types
    terminal-size text time transformers vector word8
  ];
  executableHaskellDepends = [
    ansi-terminal async attoparsec base bytestring cassava containers
    data-default directory extra filepath hermes-json lock-file
    MemoTrie optics relude safe stm streamly-core strict strict-types
    terminal-size text time transformers typed-process unix word8
  ];
  testHaskellDepends = [
    ansi-terminal async attoparsec base bytestring cassava containers
    data-default directory extra filepath hermes-json HUnit lock-file
    MemoTrie optics random relude safe stm streamly-core strict
    strict-types terminal-size text time transformers typed-process
    word8
  ];
  doHaddock = false;
  homepage = "https://github.com/maralorn/nix-output-monitor";
  description = "Processes output of Nix commands to show helpful and pretty information";
  license = lib.licenses.agpl3Plus;
  mainProgram = "nom";
}
