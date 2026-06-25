#!/usr/bin/env bash

set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
output="$root/assets/cards"

column_one="$root/Images/httpssteamusercontentaakamaihdnetugc24562292691222851304B0975D8F2C6E6649E8FC5BE3BFA5BFDBEE54E72.png"
column_two="$root/Images/httpssteamusercontentaakamaihdnetugc2456229269122280894950201A8C2B223166B57A183E7EE1D466FD8AACE.png"
column_three="$root/Images/httpssteamusercontentaakamaihdnetugc2456229269122249049F7414C4211B93BF0122F247ECF22A8FD7E5DAED5.png"

mkdir -p "$output"

crop_card() {
  local source="$1"
  local row="$2"
  local column="$3"
  local card_id="$4"

  magick "$source" \
    -crop "1181x787+$((column * 1181))+$((row * 787))" \
    +repage \
    -quality 88 \
    "$output/$card_id.webp"
}

# Column one: twelve Core cards followed by five Alternate cards.
column_one_cards=(
  C001 C004 C007 C010 C013
  C016 C019 C022 C025 C028
  C031 C034 A02 A03 A05
  A06 A08
)

# Column two: twelve Core cards followed by one Alternate card.
column_two_cards=(
  C002 C005 C008 C011 C014
  C017 C020 C023 C026 C029
  C032 C035 A04
)

# Column three: twelve Core cards followed by two Alternate cards.
column_three_cards=(
  C003 C006 C009 C012 C015
  C018 C021 C024 C027 C030
  C033 C036 A01 A07
)

for index in "${!column_one_cards[@]}"; do
  crop_card "$column_one" "$((index / 5))" "$((index % 5))" "${column_one_cards[$index]}"
done

for index in "${!column_two_cards[@]}"; do
  crop_card "$column_two" "$((index / 5))" "$((index % 5))" "${column_two_cards[$index]}"
done

for index in "${!column_three_cards[@]}"; do
  crop_card "$column_three" "$((index / 5))" "$((index % 5))" "${column_three_cards[$index]}"
done

jq '
  [
    (.C | to_entries[] | {
      id: ("C" + .key),
      name: .value.Cname,
      column: .value.column,
      image: ("/assets/cards/C" + .key + ".webp")
    }),
    (.A | to_entries[] | {
      id: ("A" + .key),
      name: .value.Aname,
      column: .value.column,
      image: ("/assets/cards/A" + .key + ".webp")
    })
  ]
' "$root/cards.json" > "$output/manifest.json"

echo "Wrote ${#column_one_cards[@]} column-one cards."
echo "Wrote ${#column_two_cards[@]} column-two cards."
echo "Wrote ${#column_three_cards[@]} column-three cards."
