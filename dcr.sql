-- This script extracts data from dcrdata, Decred block explorer, to assess Decred Project governance
-- Author: Marcelo Martins (stakey.club)
-- Version: 0.1

-- # Daily statistics

-- ## PoS rewards

COPY (
SELECT split_part(CAST (date_trunc('day', transactions.block_time) AS text), ' ', 1) "day",
round(sum(vins.value_in)/ 100000000::numeric, 8) AS sum_pos_reward
FROM transactions
LEFT JOIN vins ON transactions.tx_hash = vins.tx_hash
LEFT JOIN votes ON transactions.tx_hash = votes.tx_hash
WHERE vins.prev_tx_hash = '0000000000000000000000000000000000000000000000000000000000000000'
AND transactions.is_mainchain
AND transactions.tx_type = 2
GROUP BY 1
ORDER BY 1)
TO '/tmp/psql_daily_pos-rewards.csv' CSV HEADER;


-- ## Transactions count, volume

COPY (
SELECT split_part(CAST (date_trunc('day', transactions.block_time) AS text), ' ', 1) "day",
count(*) AS tx_count,
round(sum(transactions.sent)/ 100000000::numeric, 8) AS sum_sent,
round(sum(transactions.fees)/ 100000000::numeric, 8) AS sum_fees
FROM transactions 
WHERE transactions.fees > 0
AND transactions.is_mainchain
GROUP BY 1 
ORDER BY 1)
TO '/tmp/psql_daily_transactions-volume.csv' CSV HEADER;


-- ## Block count

COPY (
SELECT split_part(CAST (date_trunc('day', blocks.time) AS text), ' ', 1) "day",
COUNT(DISTINCT blocks.height) AS block_count
FROM blocks
WHERE blocks.is_mainchain
GROUP BY 1
ORDER BY 1)
TO '/tmp/psql_daily_block-count.csv' CSV HEADER;


-- ## Block count, historical average (count) per day

COPY (
SELECT split_part(CAST (date_trunc('day', blocks.time) AS text), ' ', 1) "day",
COUNT(*) AS block_count
FROM blocks
WHERE blocks.is_mainchain
GROUP BY 1
UNION ALL 
  SELECT 'avg = ', round(avg(a.block_count), 2) FROM 
  (SELECT split_part(CAST (date_trunc('day', blocks.time) AS text), ' ', 1) "day",
   COUNT(*) AS block_count 
   FROM blocks
   WHERE blocks.is_mainchain
   GROUP BY 1) a
ORDER BY 1)
TO '/tmp/psql_daily_block-time-historical-average.csv' CSV HEADER;


-- ## PoS reward per ticket / per block / per day

COPY (
SELECT tickets.tx_hash, tickets.block_height, tickets.spend_height,
(tickets.spend_height - tickets.block_height) AS waited_blocks,
tickets.price, votes.vote_reward,
split_part(CAST (date_trunc('day', votes.block_time) AS text), ' ', 1) "day"
FROM tickets
LEFT JOIN votes ON tickets.tx_hash = votes.ticket_hash
WHERE tickets.is_mainchain
AND tickets.spend_type = 2
ORDER BY 3)
TO '/tmp/psql_block_pos-reward-time-ticket-blocks.csv' CSV HEADER;


-- ## Ticket price / pool size / pool value

COPY (
SELECT split_part(CAST (date_trunc('day', blocks.time) AS text), ' ', 1) "day",
round(avg(tickets.price)::numeric, 8) AS avg_price,
round(avg(stats.pool_size)::numeric, 0) AS avg_pool,
round(avg(pool_val / 100000000)::numeric, 8) AS avg_pool_dcr
FROM tickets
LEFT JOIN blocks ON tickets.block_hash = blocks.hash
LEFT JOIN stats ON tickets.block_height = stats.height
WHERE tickets.is_mainchain
GROUP BY 1
ORDER BY 1)
TO '/tmp/psql_daily_ticket-price_pool-size.csv' CSV HEADER;


-- ## Coin Issuance / Supply

COPY (
SELECT x.day,
x.total_reward AS sum_total_reward,
sum(x.total_reward) OVER (ORDER BY x.day ASC) as sum_reward
FROM (
  SELECT t.day,
  sum(t.total_reward) as total_reward
  FROM (
    SELECT split_part(CAST (date_trunc('day', transactions.block_time) AS text), ' ', 1) "day", 
    transactions.block_height, 
    round(sum(vins.value_in)/100000000, 8) as total_reward
    FROM vins
    JOIN transactions ON vins.tx_hash = transactions.tx_hash
    WHERE vins.prev_tx_hash = '0000000000000000000000000000000000000000000000000000000000000000'
    AND NOT (vins.is_valid = false)
    AND vins.is_mainchain
    GROUP BY 1,2
    ORDER BY 2 ASC
  ) AS t
  GROUP BY 1
) AS x)
TO '/tmp/psql_daily_coin-issuance.csv' CSV HEADER;


-- ## Average voters per block per day, average difficulty and hashpower (GH/s) per day

COPY (
SELECT split_part(CAST (date_trunc('day', blocks.time) AS text), ' ', 1) "day",
round(avg(blocks.voters::numeric), 0) AS avg_block_voters,
round(avg(blocks.pool_size)::numeric, 0) AS avg_block_poolsize,
round(avg(blocks.difficulty)::numeric, 8) AS avg_block_difficulty,
round(avg(blocks.difficulty*(2^32)/300/1000000000)::numeric, 8) AS avg_hashrate
FROM blocks
WHERE blocks.is_mainchain 
GROUP BY 1
ORDER BY 1)
TO '/tmp/psql_daily_voters_difficulty.csv' CSV HEADER;


-- ## Network funding per day

COPY (
SELECT split_part(CAST (date_trunc('day', transactions.block_time) AS text), ' ', 1) "day",
round(sum(vouts.value)/100000000::numeric, 8) AS netfund_reward
FROM transactions 
JOIN vins ON transactions.tx_hash = vins.tx_hash 
JOIN vouts ON transactions.tx_hash = vouts.tx_hash 
WHERE vins.prev_tx_hash = '0000000000000000000000000000000000000000000000000000000000000000' 
AND vouts.script_addresses = '{Dcur2mcGjmENx4DhNqDctW5wJCVyT3Qeqkx}' 
AND transactions.is_mainchain
GROUP BY 1
ORDER BY 1)
TO '/tmp/psql_daily_network-funding.csv' CSV HEADER;


-- ## Fees, difficulty, transaction count per day

COPY (
SELECT split_part(CAST (date_trunc('day', transactions.block_time) AS text), ' ', 1) "day",
round(sum(transactions.fees) / 100000000::numeric, 8) as sum_fees,
blocks.difficulty,
count(*) as count_tx
FROM transactions
LEFT JOIN blocks ON blocks.height = transactions.block_height
WHERE transactions.fees > 0
AND transactions.is_mainchain
GROUP BY 1,3
ORDER BY 1)
TO '/tmp/psql_daily_fees-difficulty-txcount.csv' CSV HEADER;


-- # Block statistics

-- ## Transactions count, volume

COPY (
SELECT transactions.block_height,
count(*) AS tx_count,
sum(transactions.sent) AS sum_sent,
sum(transactions.fees) AS sum_fees
FROM transactions 
WHERE transactions.is_mainchain
GROUP BY 1 
ORDER BY 1)
TO '/tmp/psql_block_transactions-volume.csv' (DELIMITER ',');


-- ## Ticket price / pool size / pool value

COPY (
SELECT blocks.height,
round(avg(tickets.price)::numeric, 8) AS avg_price,
round(avg(stats.pool_size)::numeric, 0) AS avg_pool,
round(avg(pool_val / 100000000)::numeric, 8) AS avg_pool_dcr
FROM tickets
LEFT JOIN blocks ON tickets.block_hash = blocks.hash
LEFT JOIN stats ON tickets.block_height = stats.height
WHERE tickets.is_mainchain
GROUP BY 1
ORDER BY 1)
TO '/tmp/psql_block_ticket-price_pool-size.csv' CSV HEADER;


-- ## Coin Issuance / Supply (total reward)

COPY (
SELECT t.block_time, 
t.block_height, 
t.total_reward AS sum_total_reward,
round(sum(t.total_reward) OVER (ORDER BY t.block_time ASC), 8) as acum_total_reward
FROM (
  SELECT vins.block_time, 
  transactions.block_height, 
  round(sum(vins.value_in)/100000000, 8) as total_reward
  FROM vins
  JOIN transactions ON vins.tx_hash = transactions.tx_hash
  WHERE vins.prev_tx_hash = '0000000000000000000000000000000000000000000000000000000000000000'
  AND NOT (vins.is_valid = false)
  AND vins.is_mainchain
  GROUP BY 1,2
  ORDER BY 2 ASC) 
AS t LIMIT 10)
TO '/tmp/psql_block_coin-issuance.csv' CSV HEADER;


-- ## Coin Issuance / Supply (PoS reward)

COPY (
SELECT transactions.block_time, transactions.block_height, transactions.tx_type, round(round(vins.value_in, 8)/100000000, 8) AS pos_reward, votes.block_valid
FROM transactions
JOIN vins ON transactions.tx_hash = vins.tx_hash
JOIN blocks ON blocks.height = transactions.block_height
JOIN votes ON transactions.tx_hash = votes.tx_hash
WHERE vins.prev_tx_hash = '0000000000000000000000000000000000000000000000000000000000000000' 
AND blocks.is_mainchain
ORDER BY 1)
TO '/tmp/psql_block_coin-issuance-pos_reward.csv' CSV HEADER;

-- ## Coin Issuance / Supply (Network fund)

COPY (
SELECT transactions.block_time, transactions.block_height, transactions.tx_type, round(round(vouts.value, 8)/100000000, 8) AS net_fund
FROM transactions 
JOIN vins ON transactions.tx_hash = vins.tx_hash
JOIN vouts ON transactions.tx_hash = vouts.tx_hash
WHERE vins.prev_tx_hash = '0000000000000000000000000000000000000000000000000000000000000000'
AND vouts.script_addresses = '{Dcur2mcGjmENx4DhNqDctW5wJCVyT3Qeqkx}'
AND transactions.is_mainchain
ORDER BY 1)
TO '/tmp/psql_block_coin-issuance-network_fund.csv' CSV HEADER;


-- ## Coin Issuance / Supply (PoW reward)

COPY (
SELECT transactions.block_time, transactions.block_height, transactions.tx_type, round(round(vouts.value, 8)/100000000, 8) AS pow_reward, vouts.script_addresses
FROM transactions 
JOIN vins ON transactions.tx_hash = vins.tx_hash
JOIN vouts ON transactions.tx_hash = vouts.tx_hash
WHERE vins.prev_tx_hash = '0000000000000000000000000000000000000000000000000000000000000000'
AND NOT (vouts.script_addresses = '{Dcur2mcGjmENx4DhNqDctW5wJCVyT3Qeqkx}' OR vouts.script_addresses = '{}')
AND transactions.is_mainchain
AND transactions.tx_type = 0
ORDER BY 1)
TO '/tmp/psql_block_coin-issuance-pow_reward.csv' CSV HEADER;


-- ## Voters / Difficulty

COPY (
SELECT blocks.time, blocks.height, blocks.voters, blocks.pool_size, blocks.difficulty
FROM blocks
WHERE blocks.is_mainchain 
ORDER BY blocks.height)
TO '/tmp/psql_block_voters_difficulty.csv' CSV HEADER;


-- ## Fees, difficulty, transaction count per block

COPY (
SELECT transactions.block_height,
round(sum(transactions.fees) / 100000000::numeric, 8) AS sum_fees,
blocks.difficulty, count(*) AS tx_count
FROM transactions
LEFT JOIN blocks ON blocks.height = transactions.block_height
WHERE transactions.fees > 0
AND transactions.is_mainchain
GROUP BY 1,3
ORDER BY 1)
TO '/tmp/psql_block_fees-difficulty-txcount.csv' CSV HEADER;


-- # General statistics

-- ## Voters per block, mined tickets

COPY (
SELECT voters AS voters_per_block, count(*) count
FROM blocks
WHERE blocks.is_mainchain
GROUP BY 1
ORDER BY 1 ASC)
TO '/tmp/psql_general_voters-block-count-alltime-aggregate.csv' CSV HEADER;

COPY (
SELECT voters AS voters_per_block, count(*) count
FROM blocks
WHERE blocks.is_mainchain
AND split_part(CAST (date_trunc('day', blocks.time) AS text), ' ', 1) between '2016-02-08' and '2020-02-10'
GROUP BY 1
ORDER BY 1 ASC)
TO '/tmp/psql_general_voters-block-count-2016-02-08-to-2020-02-10-aggregate.csv' CSV HEADER;

COPY (
SELECT blocks.time, blocks.height, voters AS voters_per_block
FROM blocks
WHERE blocks.is_mainchain
ORDER BY 1 ASC)
TO '/tmp/psql_general_voters_per_block-alltime.csv' CSV HEADER;

-- ## Agendas, lock-in blocks (on-chain)

COPY (
SELECT agendas.id, agendas.name, agendas.status, agendas.locked_in, blocks.time AS lockin_time, agendas.activated
FROM agendas
LEFT JOIN blocks ON blocks.height = agendas.locked_in)
TO '/tmp/psql_general_agendas-lockin.csv' CSV HEADER;

-- ## Agendas, voting results (on-chain)
COPY (
SELECT agendas.id, agendas.name,
COUNT(CASE WHEN agenda_votes.agenda_vote_choice = '0' THEN 1 ELSE NULL END) AS yes,
COUNT(CASE WHEN agenda_votes.agenda_vote_choice = '1' THEN 1 ELSE NULL END) AS abs,
COUNT(CASE WHEN agenda_votes.agenda_vote_choice = '2' THEN 1 ELSE NULL END) AS no
FROM agenda_votes
LEFT JOIN agendas ON agendas.id = agenda_votes.agendas_row_id
LEFT JOIN votes ON votes.id = agenda_votes.votes_row_id
LEFT JOIN blocks ON blocks.height = votes.height
WHERE votes.is_mainchain
AND votes.height <= agendas.locked_in
AND votes.height > agendas.locked_in - 8064
GROUP BY 1, 2
ORDER BY 1) 
TO '/tmp/psql_general_agenda_vote_results.csv' CSV HEADER;


-- ## Votes per proposal per day (off-chain)

COPY (
SELECT proposals.token, split_part(CAST (date_trunc('day', proposals.time) AS text), ' ', 1) "day",
COUNT(CASE WHEN proposal_votes.choice = 'No' THEN 1 ELSE NULL END) AS no,
COUNT(CASE WHEN proposal_votes.choice = 'Yes' THEN 1 ELSE NULL END) AS yes
FROM proposal_votes
INNER JOIN proposals on proposals.id = proposal_votes.proposals_row_id
GROUP BY 1,2
ORDER BY 1)
TO '/tmp/psql_general_vote-change.csv' CSV HEADER;


-- ## Votes per proposal (off-chain)

COPY (
SELECT proposals.token, split_part(CAST (date_trunc('year', proposals.time) AS text), '-', 1) "year",
COUNT(CASE WHEN proposal_votes.choice = 'No' THEN 1 ELSE NULL END) AS no,
COUNT(CASE WHEN proposal_votes.choice = 'Yes' THEN 1 ELSE NULL END) AS yes,
COUNT(*) AS total
FROM proposal_votes
INNER JOIN proposals on proposals.id = proposal_votes.proposals_row_id
GROUP BY 1,2
ORDER BY 1)
TO '/tmp/psql_general_vote-total-number.csv' CSV HEADER;


-- ## Historical average ticket pool size

COPY (
SELECT 'avg = ', round(avg(a.pool_size), 0) FROM 
(SELECT pool_size FROM stats) a)
TO '/tmp/psql_general_average-ticket-pool-size.csv' CSV HEADER;


-- ## Ticket time to draw

COPY (
SELECT votes.block_time, votes.ticket_hash, votes.height, tickets.block_height, votes.ticket_price, votes.vote_reward, tickets.is_split
FROM votes
LEFT JOIN tickets ON tickets.tx_hash = votes.ticket_hash 
WHERE tickets.is_mainchain
ORDER BY 1)
TO '/tmp/psql_ticket-time-draw.csv' CSV HEADER;


-- ## Revoked tickets (transactions)

COPY (
SELECT transactions.block_height, transactions.tx_hash, transactions.tx_type
FROM transactions
WHERE transactions.is_mainchain
AND transactions.is_valid
AND transactions.tx_type = 3
ORDER BY 1)
TO '/tmp/psql_ticket-revoked.csv' CSV HEADER;


-- ## Genesis block coinbase spent, airdrop users, spent or not

COPY (
SELECT vouts.script_addresses, vouts.value, transactions.tx_hash, transactions.block_height, transactions.num_vout
FROM vouts
LEFT JOIN transactions ON vouts.spend_tx_row_id = transactions.id
WHERE vouts.tx_hash = '5e29cdb355b3fc7e76c98a9983cd44324b3efdd7815c866e33f6c72292cb8be6'
AND vouts.value = 28263795424
ORDER BY 4,2)
TO '/tmp/psql_transactions-genesis_block-airdrop.csv' CSV HEADER;


-- ## Genesis block coinbase spent, airdrop users, spent likely in tickets

COPY (
SELECT vouts.script_addresses, vouts.value, transactions.tx_hash, transactions.block_height, transactions.num_vout
FROM vouts
LEFT JOIN transactions ON vouts.spend_tx_row_id = transactions.id
WHERE vouts.tx_hash = '5e29cdb355b3fc7e76c98a9983cd44324b3efdd7815c866e33f6c72292cb8be6'
AND vouts.value = 28263795424
AND transactions.num_vout > 2
ORDER BY 4,2)
TO '/tmp/psql_transactions-genesis_block-airdrop-spent-likely-tickets.csv' CSV HEADER;


-- ## Genesis block coinbase spent, founders, spent or not

COPY (
SELECT vouts.script_addresses, vouts.value, transactions.tx_hash, transactions.block_height, transactions.num_vout
FROM vouts
LEFT JOIN transactions ON vouts.spend_tx_row_id = transactions.id
WHERE vouts.tx_hash = '5e29cdb355b3fc7e76c98a9983cd44324b3efdd7815c866e33f6c72292cb8be6'
AND NOT vouts.value = 28263795424
ORDER BY 4,2)
TO '/tmp/psql_transactions-genesis_block-founders.csv' CSV HEADER;


-- ## Genesis block coinbase, founders, spent likely in tickets

COPY (
SELECT vouts.script_addresses, vouts.value, transactions.tx_hash, transactions.block_height, transactions.num_vout, unnest(transactions.vout_db_ids) spent_id
FROM vouts
LEFT JOIN transactions ON vouts.spend_tx_row_id = transactions.id
WHERE vouts.tx_hash = '5e29cdb355b3fc7e76c98a9983cd44324b3efdd7815c866e33f6c72292cb8be6'
AND NOT vouts.value = 28263795424
AND transactions.num_vout > 2
ORDER BY 4,2)
TO '/tmp/psql_transactions-genesis_block-founders-spent-likely-tickets.csv' CSV HEADER;
