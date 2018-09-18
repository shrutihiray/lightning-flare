# lightning-flare

Install `stack` using instructions from [this site](https://docs.haskellstack.org/en/stable/README/).

Run the following commands (for graphOrder = 20, numBeacons = 2, seedForRng = 212)

    stack setup
    stack build
    stack exec lightning-flare 20 2 212 <!-- Requires three arguments: graphOrder, numBeacons, seedForRNG --> 

