static LIST_HEAD(einjected);
static LIST_HEAD(pci_bus_ops_list);

aer_error_init() {
        .
        .
        .
        .
}

__find_aer_error() {
        list_for_each_entry(err, &einjected, list) {
                if * return;
        }
        return;
}

__find_aer_error_by_dev() {
        return __find_aer_error();
}

/* inject_lock must be held before calling */
__find_pci_bus_ops() {
        list_for_each_entry(bus_ops, &pci_bus_ops_list, list) {
                if * return;
        }
        return;
}

pci_bus_ops_pop() {
        .
        if (list_empty(&pci_bus_ops_list))
                bus_ops = NULL;
        else {
                .
                list_del(lh);
                bus_ops = NOTNULL;
        }
        .
        return bus_ops;
}

find_pci_config_dword() {
        if * return;

        switch * {
        case: .
              .
              break;
        case: .
              .
              break;
        case: .
              break;
        case: .
              break;
        case: .
              break;
        case: .
              break;
        case: .
              .
              break;
        case: .
              break;
        }
        if * .;
        return;
}

pci_read_aer() {
        .
        if * goto out;
        __find_aer_error();
        if * goto out;
        find_pci_config_dword();
        if * {
                .
                .
                return;
        }
out:
        __find_pci_bus_ops();
        .
        return;
}

pci_write_aer() {
        .
        if * goto out;
        __find_aer_error();
        if * goto out;
        find_pci_config_dword();
        if * {
                if * .
                else .;
                .
                return;
        }
out:
        __find_pci_bus_ops(bus);
        .
        return;
}

pci_bus_ops_init() {
        .
        .
        .
}

pci_bus_set_aer_ops() {
        bus_ops = kmalloc(sizeof(*bus_ops), GFP_KERNEL);
        .
        .
        if * goto out;
        pci_bus_ops_init();
        list_add(&bus_ops->list, &pci_bus_ops_list);
        bus_ops = 0;
out:
        .
        kfree(bus_ops);
        return;
}

pcie_find_root_port() {
        while (1) {
                if * break;
                if * return;
                if * break;
                .
        }
        return;
}

find_aer_device_iter() {
        .
        if * {
                .
                if * {
                        .
                        return;
                }
        }
        return;
}

find_aer_device() {
        return .
}

aer_inject() {
        .
        .
        pci_get_bus_and_slot();
        if * return;
        pcie_find_root_port();
        if * {
                .
                goto out_put;
        }

        .
        if * {
                ret = -EIO;
                goto out_put;
        }
        .
        .
        .
        .
        if * {
                .
                goto out_put;
        }

        err_alloc =  kzalloc(sizeof(struct aer_error), GFP_KERNEL);
        rperr_alloc =  kzalloc(sizeof(struct aer_error), GFP_KERNEL);
        __find_aer_error_by_dev(dev);
        if * {
                .
                err_alloc = NULL;
                aer_error_init();
                list_add(&err->list, &einjected);
        }
        .
        .
        .
        .
        .
        .

        if * {
                .
                .
                goto out_put;
        }
        if * {
                .
                .
                goto out_put;
        }

        __find_aer_error_by_dev(rpdev);
        if * {
                .
                rperr_alloc = NULL:
                aer_error_init();
                list_add(&rperr->list, &einjected);
        }
        if * {
                if * .
                else .;
                .
                .
        }
        if * {
                if * .;
                if * {
                        .
                        if * .;
                } else .;
                .
                .
                .
        }
        .

        pci_bus_set_aer_ops(dev->bus);
        if * goto out_put;
        pci_bus_set_aer_ops(rpdev->bus);
        if * goto out_put;

        find_aer_device();
        if * {
                if * {
                        .
                        goto out_put;
                }
                .
        } else .
out_put:
        kfree(err_alloc);
        kfree(rperr_alloc);
        .
        return;
}

aer_inject_write() {
        if * return;
        if * return;
        if * return;
        aer_inject();
        return;
}

aer_inject_init() {
        .
}

aer_inject_exit() {
        .
        while (pci_bus_ops_pop()) {
                .
                kfree(bus_ops);
        }
        .
        list_for_each_entry_safe(err, err_next, &pci_bus_ops_list, list) {
                list_del(&err->list);
                kfree(err);
        }
        .
}

module_init(aer_inject_init);
module_exit(aer_inject_exit);

