package com.fy.navi.fsa;

import android.util.Log;

import com.gm.fsa.service.catalog.FSACatalog;
import com.gm.fsa.service.catalog.FSAEvent;

public class FsaServiceEvent extends FSAEvent {

    public FsaServiceEvent(int functionId) {
        super(functionId, true, FSACatalog.TransportPath.TCP, true);
    }

    @Override
    public byte[] serializeToGPB() {
        return new byte[0];
    }

    @Override
    public Integer getSerializedSize() {
        return null;
    }

    @Override
    public void onSubscriberChanged(String ip, boolean isSubscriber) {
        Log.d("FsaServiceEvent", "onSubscriberChanged: ip = " + ip + ", functionId = " + getFunctionID() + ", isSubscriber = " + isSubscriber);
        if (isSubscriber && getFunctionID() == FsaConstant.FSA_FUNCTION.ID_WHOLE_SPEED_LIMIT) {
            MyFsaService.getInstance().sendEventToMap(FsaConstant.FSA_FUNCTION.ID_WHOLE_SPEED_LIMIT, "");
        }
    }

}
