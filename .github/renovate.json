{
  "$schema": "https://docs.renovatebot.com/renovate-schema.json",
  "extends": [
    "config:recommended",
    "helpers:pinGitHubActionDigests",
    ":pinDevDependencies",
    ":pinDigestsDisabled"
  ],
  "packageRules": [
    {
      "matchFileNames": [
        ".github/**/*.yml",
        ".tool-versions"
      ],
      "groupName": "dev tools"
    },
    {
      "matchFileNames": [
        "rebar.config"
      ],
      "groupName": "rebar.config deps"
    },
    {
      "matchFileNames": [
        "package.json",
        ".nvmrc"
      ],
      "groupName": "package.json + .nvmrc deps"
    },
    {
      "matchFileNames": [
        "Dockerfile"
      ],
      "groupName": "Docker deps"
    },
    {
      "matchPackagePrefixes": [
        "minimum_otp_vsn"
      ],
      "enabled": false
    }
  ],
  "customManagers": [
    {
      "description": "Match versions (per datasource and depName) in .github/**/*.yml",
      "customType": "regex",
      "fileMatch": [
        ".github/.*/.*\\.yml"
      ],
      "matchStrings": [
        "# renovate datasource: (?<datasource>[^,]+), depName: (?<depName>[^\\n]+)\\n.+?(?<currentValue>v?\\d+(\\.\\d+(\\.\\d+)?)?(-[^\\n]+)?)\\n"
      ]
    },
    {
      "description": "Match versions in rebar.config",
      "customType": "regex",
      "fileMatch": [
        "rebar.config"
      ],
      "datasourceTemplate": "hex",
      "matchStrings": [
        "{(?<depName>[^,]+), \"(?<currentValue>\\d+\\.\\d+(\\.\\d+)?)\""
      ],
      "versioningTemplate": "semver"
    },
    {
      "description": "Match versions (per datasource and depName) in Dockerfile",
      "customType": "regex",
      "fileMatch": [
        "Dockerfile"
      ],
      "matchStrings": [
        "# renovate datasource: (?<datasource>[^,]+), depName: (?<depName>[^\\n]+)\\nENV .+?_VERSION=\"(?<currentValue>[^\"]+)\""
      ],
      "versioningTemplate": "loose"
    }
  ]
}
