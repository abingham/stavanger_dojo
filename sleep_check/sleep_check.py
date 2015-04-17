# any night where no sleep 7 hours between 10 and 8 is bad.

from datetime import datetime, time, timedelta, timezone


def read_dates(infile):
    """Generate a sequence of datetime objects parsed from `infile`.

    Dates are in original TZ.
    """
    with open(infile, 'rt') as f:
        yield from [datetime.strptime(line.strip()[1:-1],
                                      '%Y-%m-%dT%H:%M:%S.000%z')
                    for line in f]


def generate_around(mail_times, hour):
    """Generate a sequence of dusks/dawns surrounding `start` and `stop`.
    """
    start_date = (mail_times[0] - timedelta(days=1)).date()
    stop_date = (mail_times[-1] + timedelta(days=1)).date()

    date = start_date
    while date <= stop_date:
        # We provide a dummy tzinfo so that it's not naive. This is
        # important for future calculations.
        yield datetime.combine(date, time(hour=hour,
                                          tzinfo=timezone(timedelta())))
        date = date + timedelta(days=1)


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
            yield datetime.combine(ts.date(), new_time), evt
        else:
            curr_tz = ts.tzinfo
            yield ts, evt


def nights(stream):
    """Generate a sequence of tuples of (timestamp, event) from `stream`.

    `stream` must be a sequence of (timestamp, event) tuples.

    Each generated tuple represents a single night. The first entry
    will be a DUSK and the last will be a DAWN. All intervening events
    will be MAIL.
    """
    stamps = []
    for ts, evt in update_timestamps(full_stream,
                                     full_stream[0][0].tzinfo):
        if evt == 'DUSK':
            stamps = []

        stamps.append((ts, evt))

        if evt == 'DAWN':
            yield stamps


def check_night(night, required_sleep):
    """Return True if enough sleep was had on `night`, else return False.

    """
    # TODO: Use islice?
    deltas = [y[0] - x[0] for x, y in zip(night, night[1:])]
    delta_hours = list(map(lambda td: td.seconds / 60 / 60, deltas))
    return any(map(lambda d: d >= required_sleep, delta_hours))


def run():
    just_dates = sorted(read_dates("maildatoer.txt"))
    mails = [(ts, 'MAIL') for ts in just_dates]
    dawns = [(ts, 'DAWN') for ts in generate_around(just_dates, 8)]
    dusks = [(ts, 'DUSK') for ts in generate_around(just_dates, 22)]

    full_stream = sorted(mails + dawns + dusks, key=lambda t: t[0])

    sleepless_nights = list(filter(lambda n: not check_night(n, 7), nights(full_stream)))
    for n in sleepless_nights:
        print('NO SLEEP on {} due to {}'.format(n[0][0], n))
    print(len(sleepless_nights))

run()
