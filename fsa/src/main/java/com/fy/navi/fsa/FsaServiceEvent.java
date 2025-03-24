package com.fy.navi.fsa;

import android.util.Log;

import com.gm.fsa.service.catalog.FSACatalog;
import com.gm.fsa.service.catalog.FSAEvent;

public final class FsaServiceEvent extends FSAEvent {

    public FsaServiceEvent(final int functionId) {
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
    public void onSubscriberChanged(final String ip, final boolean isSubscriber) {
        Log.d("FsaServiceEvent", "onSubscriberChanged: ip = " + ip
                + ", functionId = " + getFunctionID() + "-" + FsaIdString.function2String(getFunctionID())
                + ", isSubscriber = " + isSubscriber);
        if (isSubscriber) {
            MyFsaService.getInstance().subscribeEvent(getFunctionID(), ip);
        } else {
            MyFsaService.getInstance().unsubscribeEvent(getFunctionID(), ip);
        }
        if (isSubscriber && getFunctionID() == FsaConstant.FsaFunction.ID_WHOLE_SPEED_LIMIT) {
            MyFsaService.getInstance().sendEventToMap(FsaConstant.FsaFunction.ID_WHOLE_SPEED_LIMIT, "");
        }
    }

}
