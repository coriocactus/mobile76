#!/bin/python

import re
import sys
import os
import logging
from datetime import datetime

LOGFILENAME = f'{datetime.today().strftime("%y%m%d")}-{os.getpid()}.log'

handle = logging.FileHandler(LOGFILENAME)
handle.setFormatter(
    logging.Formatter('%(asctime)s | %(levelname)s | %(message)s')
)

root_log = logging.getLogger()
root_log.setLevel(logging.INFO)
root_log.addHandler(handle)


class OfficerRecord:

    def __init__(self, record):
        self.record = record.replace(',', '').replace('C/O', '')
        self.check_register()
        self.parse_row()
        self.parse_person_info()
        self.parse_address()
        self.output_record()

    def check_register(self):

        register_pattern = r'^(?P<cid>\d{8})(?P<pid>\d{12})\s+(?P<doa>\d{8})'
        check_reg = re.search(register_pattern, self.record)

        if check_reg:
            self.register = check_reg.groupdict()
        else:
            root_log.critical(
                f'overfitted (register) | {self.record}'
            )

    def parse_row(self):

        register_str = [char for char in self.record[:43] if char.isdigit()]

        if len(register_str) != 28:
            root_log.critical(
                f'overfitted (register presentation) | {self.record}'
            )

        fields = self.record[43:].split('<')

        if len(fields) != 11:
            root_log.critical(
                f'overfitted (fields presentation) | f{self.record}'
            )
        else:
            self.fields = fields

    def parse_person_info(self):

        personal = dict(
            zip(('fname', 'lname', 'hon', 'cof'), self.fields[1:5])
        )

        dob_pattern = r'(?P<dob>\d{6})'
        dob_reg = re.search(dob_pattern, self.fields[0])
        dob = dob_reg.groupdict() if dob_reg else {'dob': ''}
        title_pattern = r'(?P<title>[A-Z]{1,4})\.?$'
        title_reg = re.search(title_pattern, self.fields[0])
        title = title_reg.groupdict() if title_reg else {'title': ''}

        self.person_info = personal | dob | title

    def parse_address(self):

        address = dict(
            zip(('po', 'ad1', 'ad2', 'town', 'cnty', 'ctry'), self.fields[5:])

        )

        dob = self.person_info['dob'] if self.person_info['dob'] else ''
        title = self.person_info['title'] if self.person_info['title'] else ''
        remaining = self.fields[0].replace(dob, '').replace(title, '')
        pc = {'pc': remaining.replace('.', '')}

        self.address = address | pc

    def output_record(self):

        parsed_record = self.register | self.person_info | self.address
        print(','.join([v.strip() for v in parsed_record.values()]))

        root_log.info(
            f'recorded: {self.register["cid"]} {self.register["pid"]}'
        )


if __name__ == '__main__':

    for row in sys.stdin:
        OfficerRecord(row)
