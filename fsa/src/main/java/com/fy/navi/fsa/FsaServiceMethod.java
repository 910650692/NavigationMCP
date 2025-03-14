package com.fy.navi.fsa;

import android.util.Log;

import com.gm.fsa.service.FSAMessage;
import com.gm.fsa.service.FSAServiceConnectedClient;
import com.gm.fsa.service.NotificationObject;
import com.gm.fsa.service.catalog.FSAMethod;

import java.nio.charset.StandardCharsets;

public class FsaServiceMethod extends FSAMethod {


    private IRequestReceiveListener mListener;

    public FsaServiceMethod(IRequestReceiveListener listener) {
        super(FsaConstant.FSA_METHOD.ID_REQUEST_MSG, true);
        mListener = listener;
    }

    @Override
    public void onRequest(byte[] bytes) {}

    @Override
    public void onRequest(NotificationObject notificationObject) {
        if (null == notificationObject || null == notificationObject.msg) {
            Log.e(FsaConstant.FSA_TAG, "NotificationObject is empty");
            return;
        }

        FSAMessage fsaMessage = notificationObject.msg;
        int functionId = fsaMessage.getFunctionId();
        String payload = new String(fsaMessage.getPayload(), StandardCharsets.UTF_8);
        Log.d(FsaConstant.FSA_TAG, "receiveRequest, functionId: " + functionId + ", payload: " + payload);
        if (null != mListener) {
            mListener.onReceiveRequest(functionId, payload);
        }
    }

    @Override
    public void onRequestResponse(byte[] bytes, FSAServiceConnectedClient fsaServiceConnectedClient, Integer integer) {}

    @Override
    public void onRequestResponse(NotificationObject notificationObject) {}

    @Override
    public byte[] serializeResponseToGPB() {
        return new byte[0];
    }

    @Override
    public void deserializeRequestFromGPB(byte[] bytes) {}

    @Override
    public void deserializeRequestResponseFromGPB(byte[] bytes) {}

    @Override
    public Integer getSerializedSize() {
        return 0;
    }

    public interface IRequestReceiveListener {
        void onReceiveRequest(int functionId, String payload);
    }
}
