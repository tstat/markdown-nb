#### Protocol

- On connect, the server sends the entire document (UTF-8 encoded bytes; not JSON)
- After, the server accepts and sends document deltas that look like:

  ```
  { 
    "add": {
      "off": 10,    // codepoint-offset into document
      "txt": "foo"  // bytes to insert
    }
  }
  ```
  ```
  {
    "del": {
      "off": 10, // codepoint-offset into document
      "len": 5   // number of codepoints to delete
    }
  }
  ```

#### Building

    ./build
    
#### Running

    ./run

#### `ghcid` development

    ./dev
