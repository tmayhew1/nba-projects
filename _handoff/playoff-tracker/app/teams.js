// Team rosters, bracket structure, and round constants for the current season.

export const TEAMS = {
  SAS: { name: "Spurs",    seed: 2, owner: "Spencer", conf: "W" },
  DEN: { name: "Nuggets",  seed: 3, owner: "Spencer", conf: "W" },
  CLE: { name: "Cavs",     seed: 4, owner: "Spencer", conf: "E" },
  HOU: { name: "Rockets",  seed: 5, owner: "Spencer", conf: "W" },
  NYK: { name: "Knicks",   seed: 3, owner: "Spencer", conf: "E" },
  ORL: { name: "Magic",    seed: 8, owner: "Spencer", conf: "E" },
  PHI: { name: "76ers",    seed: 7, owner: "Spencer", conf: "E" },
  PHX: { name: "Suns",     seed: 8, owner: "Spencer", conf: "W" },
  OKC: { name: "Thunder",  seed: 1, owner: "Trey",    conf: "W" },
  BOS: { name: "Celtics",  seed: 2, owner: "Trey",    conf: "E" },
  MIN: { name: "Wolves",   seed: 6, owner: "Trey",    conf: "W" },
  DET: { name: "Pistons",  seed: 1, owner: "Trey",    conf: "E" },
  ATL: { name: "Hawks",    seed: 6, owner: "Trey",    conf: "E" },
  LAL: { name: "Lakers",   seed: 4, owner: "Trey",    conf: "W" },
  TOR: { name: "Raptors",  seed: 5, owner: "Trey",    conf: "E" },
  POR: { name: "Blazers",  seed: 7, owner: "Trey",    conf: "W" },
};

export const BRACKET = {
  r1: [
    { id: "E1", teams: ["DET", "ORL"], conf: "E" },
    { id: "E4", teams: ["CLE", "TOR"], conf: "E" },
    { id: "E3", teams: ["NYK", "ATL"], conf: "E" },
    { id: "E2", teams: ["BOS", "PHI"], conf: "E" },
    { id: "W1", teams: ["OKC", "PHX"], conf: "W" },
    { id: "W4", teams: ["LAL", "HOU"], conf: "W" },
    { id: "W3", teams: ["DEN", "MIN"], conf: "W" },
    { id: "W2", teams: ["SAS", "POR"], conf: "W" },
  ],
  r2: [
    { id: "ES1", from: ["E1", "E4"], conf: "E" },
    { id: "ES2", from: ["E2", "E3"], conf: "E" },
    { id: "WS1", from: ["W1", "W4"], conf: "W" },
    { id: "WS2", from: ["W2", "W3"], conf: "W" },
  ],
  r3: [
    { id: "ECF", from: ["ES1", "ES2"], conf: "E" },
    { id: "WCF", from: ["WS1", "WS2"], conf: "W" },
  ],
  r4: [{ id: "F", from: ["ECF", "WCF"], conf: "F" }],
};

export const ROUND_BASE = { r1: 1, r2: 2, r3: 4, r4: 8 };
export const ROUND_LABEL = { r1: "First Round", r2: "Conf Semis", r3: "Conf Finals", r4: "Finals" };
export const STORAGE_KEY = "playoff-draft-v1";
