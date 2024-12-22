import re

class FormatError(Exception):
    pass

class UndoError(Exception):
    pass

class RedoError(Exception):
    pass

class DomainSet(set):
    def __init__(self, *args):
        super().__init__()
        self.history = []
        self.redo_stack = []
        if args:
            for item in args[0]:
                self.add(item)

    def _is_valid_domain(self, value):
        pattern = r'^(?:[a-zA-Z0-9-]+\.)+[a-zA-Z]{2,}$'
        return re.match(pattern, value)

    def add(self, value):
        if not isinstance(value, str):
            raise TypeError("Only strings are allowed")
        if not self._is_valid_domain(value):
            raise FormatError("Invalid domain name format")
        
        self.history.append(self.copy())
        self.redo_stack.clear()
        super().add(value)

    def remove(self, value):
        if value not in self:
            raise KeyError(f"{value} not found in set")
        
        self.history.append(self.copy())
        self.redo_stack.clear()
        super().remove(value)

    def undo(self):
        if not self.history:
            raise UndoError("No actions to undo")
        self.redo_stack.append(self.copy())
        previous_state = self.history.pop()
        self.clear()
        super().update(previous_state)

    def redo(self):
        if not self.redo_stack:
            raise RedoError("No actions to redo")
        self.history.append(self.copy())
        next_state = self.redo_stack.pop()
        self.clear()
        super().update(next_state)
