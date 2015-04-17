# any night where no sleep 7 hours between 10 and 8

# read stream of timestamps labelled MAIL, DUSK, and DAWN (we generate
# the second and third types.)

# On a MAIL or a DUSK, we record that timestamp as "sleep started"
# On DAWN we simply subtract the dawn timestamp from "sleep started".
# If this is less than 7, we say not enough sleep.

import datetime


THE_TZ = datetime.timezone(datetime.timedelta())

def read_dates(infile):
    """Generate a sequence of datetime objects parsed from `infile`.

    Dates are in original TZ.
    """
    with open(infile, 'rt') as f:
        for line in f:
            line = line.strip()[1:-1]
            dt = datetime.datetime.strptime(line, '%Y-%m-%dT%H:%M:%S.000%z')
            yield dt


def generate_around(mail_times, hour):
    """Generate a sequence of dusks/dawns surrounding `start` and `stop`.
    """
    start_date = (mail_times[0] - datetime.timedelta(days=1)).date()
    stop_date = (mail_times[-1] + datetime.timedelta(days=1)).date()

    date = start_date
    while date <= stop_date:
        yield datetime.datetime.combine(date, datetime.time(hour=hour,
                                                            tzinfo=THE_TZ))
        date = date + datetime.timedelta(days=1)


def update_timestamps(events, initial_tz):
    """Set the timezone for each DAWN or DUSK event to the previous mail.

    Any sleep events with no preceding mail get the TZ of the first mail.

    Generates the updated sequence.

    TODO: What if there is no mail?
    TODO: Combine with generate_around.
    """
    curr_tz = initial_tz
    for ts, evt in events:
        if evt in ('DAWN', 'DUSK'):
            new_time = ts.time()
            new_time = new_time.replace(tzinfo=curr_tz)
            yield datetime.datetime.combine(ts.date(), new_time), evt
        else:
            curr_tz = ts.tzinfo
            yield ts, evt


def check_night(sequence):
    # TODO: Make this work for general sequences if possible, not just
    # lists.
    deltas = [y - x for x, y in zip(sequence, sequence[1:])]
    delta_hours = list(map(lambda td: td.seconds / 60 / 60, deltas))
    if not any(map(lambda d: d >= 7, delta_hours)):
        print("NO SLEEP {} due to {}".format(sequence[0], delta_hours))

just_dates = sorted(read_dates("maildatoer.txt"))
mails = [(ts, 'MAIL') for ts in just_dates]
dawns = [(ts, 'DAWN') for ts in generate_around(just_dates, 8)]
dusks = [(ts, 'DUSK') for ts in generate_around(just_dates, 22)]

full_stream = sorted(mails + dawns + dusks, key=lambda t: t[0])

stamps = []
for ts, evt in update_timestamps(full_stream, full_stream[0][0].tzinfo):
    if evt == 'DUSK':
        stamps = []

    stamps.append(ts)

    if evt == 'DAWN':
        check_night(stamps)

# sleep_started = None
# for ts, cls in full_stream:
#     if cls in ('MAIL', 'DUSK'):
#         sleep_started = ts
#     elif sleep_started is not None:
#         sleep_time = ts - sleep_started
#         if (sleep_time.seconds / 60 / 60) < 7:
#             print("only {} on {}".format(sleep_time, ts))
#             print("last event at {}".format(sleep_started))

# print(mail_dates)
# for d in generate_dawns(mail_dates[0], mail_dates[-1]):
#     print(d)
