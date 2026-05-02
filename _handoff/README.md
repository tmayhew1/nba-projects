# Handoff bundle for the `nba-stats-web` build

This directory exists because the new Claude Code session that picks up the
`nba-stats-web` build has GitHub MCP scope on `nba-stats-web` and
`nba-projects` only — **not** `tmayhew1/playoff-tracker`. The plan
(`PLAN.md` at the repo root) tells the implementer to use playoff-tracker as
the framework template, so the relevant playoff-tracker source files are
snapshotted under `_handoff/playoff-tracker/` to stay in scope.

## What's here

```
_handoff/playoff-tracker/
  app/
    page.js                  # PlayoffTracker UI; see VABreakdown for Tab 8 pattern
    layout.js                # Next.js root layout
    globals.css              # Tailwind base + font import
    teams.js                 # constants module convention to mirror
    scoring.js               # LGA constants + valueAdd() — verbatim port target
    historical.js            # not strictly needed, kept for context on history tab
    api/
      scores/route.js        # Cache-Control + revalidate convention
      boxscore/route.js      # Same; also shows ISO-duration parsing
  tailwind.config.js         # exact config to mirror (theme.extend empty)
  next.config.js             # reactStrictMode only
  postcss.config.js          # tailwind + autoprefixer
  package.json               # version pins: next 14.2.5, react 18.3.1, tailwind 3.4.7
```

These are read-only references — do not commit them into the new repo.
They're a one-time snapshot taken when the handoff was prepared. If the
upstream playoff-tracker repo changes meaningfully later and you need the
updated version, re-run the snapshot.

## Where to read other reference material

- The plan: `PLAN.md` at the root of this branch.
- Shiny app source: `app.R`, `totals_collect.R`, `eda.R`, the per-tab
  `tab N - ...R` files, and the `Complete Data/` CSV directory — all on
  `main` of this same repo (`tmayhew1/nba-projects`).
- The seed CSVs to copy into the new repo: `Complete Data/Totals_s_*.csv`,
  `Totals_p_*.csv` (newest dated copy), `team_hex_colors.csv`,
  `avgsSummary.csv`, `team_abbreviations.csv`, `scoring_levels.csv`,
  `menu_options*.csv`.
