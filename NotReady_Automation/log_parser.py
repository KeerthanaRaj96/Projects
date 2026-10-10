# Jenkins Log Parser - read logs and find broken nodes

import re
from dataclasses import dataclass


@dataclass
class Node:
    # when a node first went down
    id: str
    status: str
    test: str


class JenkinsLogParser:
    # read the log and find which nodes broke
    
    def __init__(self):
        self.broken = []
        self.seen = set()
    
    def parse(self, log: str):
        # go through the log line by line
        lines = log.split('\n')
        current_test = None
        reading_nodes = False
        
        for line in lines:
            line = line.strip()
            
            # spot a test name
            if re.match(r'^[a-z]+-test-\d+$', line):
                current_test = line
                reading_nodes = False
                continue
            
            # spot the status header
            if 'STATUS' in line and ('KERNEL' in line or 'NODE' in line):
                reading_nodes = True
                continue
            
            # extract node status
            if reading_nodes and current_test:
                match = re.match(r'^([a-z]{2}\d{4})\s+(Ready|NotReady)\s*$', line)
                
                if match:
                    node_id = match.group(1)
                    status = match.group(2)
                    
                    # only report the first time a node broke
                    if status == "NotReady" and node_id not in self.seen:
                        node = Node(id=node_id, status=status, test=current_test)
                        self.broken.append(node)
                        self.seen.add(node_id)
                
                # stop reading nodes
                if not line or line.startswith('+') or line.startswith('='):
                    reading_nodes = False
    
    def show(self):
        # print the results
        if not self.broken:
            print("No NotReady nodes detected. All nodes passed all tests.")
            return
        
        print("=" * 70)
        print(f"NotReady Nodes ({len(self.broken)} nodes affected)")
        print("=" * 70)
        print("")
        
        for node in self.broken:
            print(f"Node {node.id} - {node.status} - {node.test}")
        
        print("")
        print("=" * 70)
        print(f"Summary: {len(self.broken)} unique nodes affected")
        print("=" * 70)