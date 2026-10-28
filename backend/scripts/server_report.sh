#!/usr/bin/env bash
# PDAv1 server report for the RCSB discussion.
# Run on the server hosting pragmaticproteindesign.bio.ed.ac.uk:
#   bash server_report.sh [/var/log/nginx] > pda_server_report.txt
# Prints aggregates only: no IP addresses leave the server.
set -u
LOGDIR="${1:-/var/log/nginx}"

logs() { for f in "$LOGDIR"/access.log*; do case "$f" in *.gz) zcat "$f";; *) cat "$f";; esac; done; }

echo "== Log retention"
ls -la "$LOGDIR" | grep access
echo "first/last entry:"; logs | awk '{print $4}' | tr -d '[' | sort -t/ -k3,3 -k2,2M -k1,1n | sed -n '1p;$p'

echo; echo "== Hours with zero requests (possible outages), oldest first"
logs | awk '{split($4,a,":"); print substr(a[1],2)":"a[2]}' | sort -u > /tmp/pda_hours
total=$(wc -l < /tmp/pda_hours); echo "hours with traffic: $total"
python3 - /tmp/pda_hours <<'EOF'
import sys, datetime as dt
hs = sorted(dt.datetime.strptime(l.strip(), "%d/%b/%Y:%H") for l in open(sys.argv[1]) if l.strip())
gaps = [(a, b) for a, b in zip(hs, hs[1:]) if b - a > dt.timedelta(hours=2)]
span = (hs[-1] - hs[0]).total_seconds() / 3600 + 1
print(f"span: {hs[0]} to {hs[-1]} ({span:.0f} h), hourly coverage {len(hs)/span:.1%}")
for a, b in gaps: print(f"gap: {a} -> {b} ({(b-a).total_seconds()/3600 - 1:.0f} h silent)")
EOF
rm -f /tmp/pda_hours

echo; echo "== Requests per month"
logs | awk '{split($4,a,"/"); print a[3]"-"a[2]}' | cut -c1-8 | sort | uniq -c

echo; echo "== Top 25 user agents"
logs | awk -F'"' '{print $6}' | sort | uniq -c | sort -rn | head -25

echo; echo "== Known crawlers / AI bots (request counts)"
for b in Googlebot bingbot GPTBot ClaudeBot anthropic CCBot Bytespider PerplexityBot Amazonbot meta-externalagent AhrefsBot SemrushBot python-requests curl Wget; do
  printf "%-20s %s\n" "$b" "$(logs | grep -ci "$b")"; done

echo; echo "== API traffic (/pda-api)"
logs | awk '$7 ~ /pda-api/ {split($7,p,"?"); print p[1]}' | sed 's#design-details/.*#design-details/<id>#' | sort | uniq -c | sort -rn
echo "distinct clients calling /pda-api/all-designs per month:"
logs | awk '$7 ~ /all-designs/ {split($4,a,"/"); print substr(a[3],1,4)"-"a[2], $1}' | sort -u | awk '{print $1}' | uniq -c

echo; echo "== Status codes"
logs | awk '{print $9}' | sort | uniq -c | sort -rn | head

echo; echo "== Reboots and containers"
last -x reboot 2>/dev/null | head -20 || journalctl --list-boots | tail -20
docker ps --format '{{.Names}}\t{{.Status}}'
for c in $(docker ps -q); do docker inspect -f '{{.Name}} started={{.State.StartedAt}} restarts={{.RestartCount}} policy={{.HostConfig.RestartPolicy.Name}}' "$c"; done
