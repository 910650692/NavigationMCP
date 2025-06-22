package com.sgm.navi.service.adapter.position.bls.listener;

import com.sgm.navi.service.define.position.LocGpgsvWrapper;

public interface IUsedSatelliteNumCallback {
    void onSatelliteNum(int num);

    void onGSVInfo(LocGpgsvWrapper wrapper);
}
