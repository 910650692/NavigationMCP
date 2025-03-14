package com.fy.navi.service.adapter.position.bls.listener;

import com.autonavi.gbl.pos.model.LocGnss;
import com.fy.navi.service.define.position.LocGpgsvWrapper;

public interface ILocationListener {

    void onGSVInfo(LocGpgsvWrapper wrapper);

    void onLocationChanged(LocGnss locGnss);
}
