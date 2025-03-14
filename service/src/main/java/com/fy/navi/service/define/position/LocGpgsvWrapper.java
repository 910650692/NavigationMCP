package com.fy.navi.service.define.position;

import com.autonavi.gbl.pos.model.LocGpgsv;

public class LocGpgsvWrapper {
    public LocGpgsv locGpgsv;
    public LocGpgsvAttributes attributes;

    public LocGpgsvWrapper() {
        locGpgsv = new LocGpgsv();
        attributes = new LocGpgsvAttributes();
    }
}
