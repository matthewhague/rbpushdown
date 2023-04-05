


__find_target_type()
{
	list_for_each(tih, &_targets) {
		list_entry(tih, struct tt_internal, list);
		if * return 1;
	}
	return 0;
}

get_target_type() {
	.
	__find_target_type();
	if * {
		if * .;
		else .;
	}
	.
	return;
}

load_module() {
	.
}

dm_get_target_type() {
	if * {
		.
		get_target_type();
	}
	return;
}

dm_put_target_type() {
	.
	if * .;
	if * .;
    .
	return;
}

*alloc_target() {
	kmalloc();
	if * {
		.
		.
	}
	return;
}

dm_register_target() {
    int rv = 0;
	alloc_target();

	.
	v = __find_target_type()
    if v {
		kfree();
		rv = -EEXIST;
	} else
		list_add(&_targets);

	.
	if (rv)
		kfree();
	return;
}

dm_unregister_target() {
	.
    v = __find_target_type();
	if v {
		.
		return 0;
	}

	if * {
		.
		return 0;
	}

	list_del(&ti->list);
	kfree();

	.
	return 1;
}


EXPORT_SYMBOL(dm_register_target);
EXPORT_SYMBOL(dm_unregister_target);
