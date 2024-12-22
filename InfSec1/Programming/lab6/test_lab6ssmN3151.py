import pytest
from lab6ssmN3151 import DomainSet, FormatError, UndoError, RedoError

def test_add_valid_domain():
    dset = DomainSet()
    dset.add("example.com")
    assert "example.com" in dset

def test_add_invalid_domain():
    dset = DomainSet()
    with pytest.raises(FormatError):
        dset.add("1312321/1123")

def test_add_non_string():
    dset = DomainSet()
    with pytest.raises(TypeError):
        dset.add(123)

def test_remove():
    dset = DomainSet(["example.com"])
    dset.remove("example.com")
    assert "example.com" not in dset

def test_remove_nonexistent():
    dset = DomainSet()
    with pytest.raises(KeyError):
        dset.remove("nonexistent.com")

def test_undo():
    dset = DomainSet(["example.com"])
    dset.add("example.org")
    dset.undo()
    assert "example.org" not in dset
    assert "example.com" in dset

def test_redo():
    dset = DomainSet(["example.com"])
    dset.add("example.org")
    dset.undo()
    dset.redo()
    assert "example.org" in dset

def test_undo_without_history():
    dset = DomainSet()
    with pytest.raises(UndoError):
        dset.undo()

def test_redo_without_history():
    dset = DomainSet()
    with pytest.raises(RedoError):
        dset.redo()

def test_undo_redo_sequence():
    dset = DomainSet(["example.com"])
    dset.add("example.org")
    dset.undo()
    dset.redo()
    assert "example.org" in dset

def test_undo_after_undo_redo():
    dset = DomainSet(["example.com"])
    dset.add("example.org")
    dset.undo()
    dset.redo()
    dset.undo()
    assert "example.org" not in dset
    assert "example.com" in dset

def test_add_multiple_valid_domains():
    dset = DomainSet()
    domains = ["example.com", "test.org", "domain.net"]
    for domain in domains:
        dset.add(domain)
    for domain in domains:
        assert domain in dset

def test_remove_multiple_domains():
    dset = DomainSet(["example.com", "test.org", "domain.net"])
    dset.remove("test.org")
    assert "test.org" not in dset
    assert "example.com" in dset
    assert "domain.net" in dset

def test_undo_after_multiple_adds():
    dset = DomainSet()
    dset.add("example.com")
    dset.add("test.org")
    dset.add("domain.net")
    dset.undo()
    assert "domain.net" not in dset
    assert "test.org" in dset
    assert "example.com" in dset

def test_redo_after_multiple_undos():
    dset = DomainSet(["example.com", "test.org", "domain.net"])
    dset.remove("test.org")
    dset.undo()
    dset.undo()
    assert "example.com" in dset
    assert "test.org" in dset
    assert "domain.net" in dset
    dset.redo()
    assert "example.com" in dset
    assert "test.org" not in dset
    assert "domain.net" in dset

def test_mixed_operations_with_undo_redo():
    dset = DomainSet()
    dset.add("example.com")
    dset.add("test.org")
    dset.remove("example.com")
    dset.undo()
    dset.add("domain.net")
    dset.redo()
    assert "example.com" in dset
    assert "test.org" in dset
    assert "domain.net" in dset

def test_redo_after_new_addition():
    dset = DomainSet(["example.com"])
    dset.add("test.org")
    dset.undo()
    dset.add("domain.net")
    with pytest.raises(RedoError):
        dset.redo()

def test_undo_redo_boundary_conditions():
    dset = DomainSet(["example.com"])
    dset.undo()
    with pytest.raises(UndoError):
        dset.undo()
    dset.redo()
    with pytest.raises(RedoError):
        dset.redo()

def test_add_multiple_valid_domains():
    dset = DomainSet()
    domains = ["example.com", "test.org", "domain.net"]
    for domain in domains:
        dset.add(domain)
    for domain in domains:
        assert domain in dset

def test_remove_multiple_domains():
    dset = DomainSet(["example.com", "test.org", "domain.net"])
    dset.remove("test.org")
    assert "test.org" not in dset
    assert "example.com" in dset
    assert "domain.net" in dset

def test_undo_after_multiple_adds():
    dset = DomainSet()
    dset.add("example.com")
    dset.add("test.org")
    dset.add("domain.net")
    dset.undo()
    assert "domain.net" not in dset
    assert "test.org" in dset
    assert "example.com" in dset

def test_redo_after_multiple_undos():
    dset = DomainSet(["example.com", "test.org", "domain.net"])
    dset.remove("test.org")
    dset.undo()
    assert "example.com" in dset
    assert "test.org" in dset
    assert "domain.net" in dset
    dset.redo()
    assert "example.com" in dset
    assert "test.org" not in dset
    assert "domain.net" in dset

def test_mixed_operations_with_undo_redo():
    dset = DomainSet()
    dset.add("example.com")
    dset.add("test.org")
    dset.remove("example.com")
    dset.undo()
    dset.add("domain.net")
    dset.undo()
    dset.redo()
    assert "example.com" in dset
    assert "test.org" in dset
    assert "domain.net" in dset

def test_redo_after_new_addition():
    dset = DomainSet(["example.com"])
    dset.add("test.org")
    dset.undo()
    dset.add("domain.net")
    with pytest.raises(RedoError):
        dset.redo()

def test_undo_redo_boundary_conditions():
    dset = DomainSet(["example.com"])
    dset.undo()
    with pytest.raises(UndoError):
        dset.undo()
    dset.redo()
    with pytest.raises(RedoError):
        dset.redo()



def test_domain_format_check():
    dset = DomainSet()
    valid_domains = ["example.com", "sub.example.co.uk", "test.org", "my-domain.net", "another.test.edu"]
    invalid_domains = ["example", "test@org", "my_domain.net", "sub.example.", "example.c"]
    
    for domain in valid_domains:
        dset.add(domain)
        assert domain in dset

    for domain in invalid_domains:
        with pytest.raises(FormatError):
            dset.add(domain)