set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"

if [ -z "${DHALL:-}" ]; then
    (
        cd "${ROOT}/standard"
        cabal build exe:dhall
    )
    DHALL="$(cd "${ROOT}/standard" && cabal list-bin exe:dhall)"
fi

# Parser success fixtures: encode the parsed expression.
find "${ROOT}/tests/parser/success" -type f -name '*A.dhall' | while read -r FILE; do
    PREFIX="${FILE%A.dhall}"
    "${DHALL}" --parse-only "${PREFIX}B.dhallb" < "${FILE}" >/dev/null
    "${DHALL}" --parse-only --diag "${PREFIX}B.diag" < "${FILE}" >/dev/null
done

# Diagnostic notation for every committed CBOR file, including hand-written
# binary-decode inputs.  Do not rewrite those *.dhallb files.
find "${ROOT}/tests" -type f -name '*.dhallb' | while read -r FILE; do
    "${DHALL}" --from-cbor --diag "${FILE%.dhallb}.diag" < "${FILE}" >/dev/null
done
