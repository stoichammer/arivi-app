Xoken Arch is a stand-alone network gateway that enables applications lacking a native Arivi-P2P library to access the Xoken P2P overlay network.  Currently the Arivi P2P implementations are limited to Haskell, and hence the alternative solution to make the Xoken P2P n/w more accessible. We do have other popular language implemetations in the road-map, especially for Java.

The Arch is very light weight & flexible deployment choices are available as shown below.


Desktop - In this deployment scenario the Arch process is deployed co-located to the application , typically as a background service / process interoperable via IPC, and it acts as a simple TCP endpoint (length prefix messages).


Server - An externally hosted gateway, with TLS support for security, and capable of handling much larger demand from thousands of remote clients. Perfect for phone-apps & web-apps to access the Xoken P2P network in the interim. Besides, some applications could prefer this approach even when native libraries are made available.

----------------------------------------------------------------------------------------------------

Arivi P2P core library supports the following:

Arivi Network Protocol: A secure network protocol akin to SSH/SCP that supports both EndPoint & UDP transports and offers the following features.
- Chunking
- Multiplexing/Demultiplexing
- End to end encryption (IES key exchange, with perfect forward secrecy)
- Authenticated messaging Poly1305-ChaCha20 (AEAD). 

P2P Protocol suite : 
- Peer Lookup/Discovery using an improved Kademlia DHT protocol. Fortified from various forms of eclipse attacks by using a novel peer verification mechanism.
- Remote Proceduce Calls; pull pattern for fetching larger payload from peers e.g. entire blocks 
- Pub-Sub messaging pattern for efficient distributed push notifications in the distributed network as an improvement over traditional gossip protocol
- Peer reputation: a comprehensive peer reputation framework to track the reputation of other peers in the network.
