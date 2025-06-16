package com.fy.navi.service.adapter.position.bls.listener;

import com.fy.navi.service.define.position.LocGpgsvWrapper;

public interface IUsedSatelliteNumCallback {
    void onSatelliteNum(int num);

    void onGSVInfo(LocGpgsvWrapper wrapper);
}
