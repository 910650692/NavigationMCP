package com.fy.navi.fsa;

import android.util.Log;

import com.gm.fsa.service.FSAMessage;
import com.gm.fsa.service.FSAServiceConnectedClient;
import com.gm.fsa.service.NotificationObject;
import com.gm.fsa.service.catalog.FSAMethod;

import java.nio.charset.StandardCharsets;

public class FsaServiceMethod extends FSAMethod {


    private IRequestReceiveListener mListener;

    public FsaServiceMethod(final IRequestReceiveListener listener) {
        super(FsaConstant.FsaMethod.ID_REQUEST_MSG, true);
        mListener = listener;
    }

    @Override
    public void onRequest(final byte[] bytes) {
    }

    @Override
    public void onRequest(final NotificationObject notificationObject) {
        if (null == notificationObject || null == notificationObject.msg) {
            Log.e(FsaConstant.FSA_TAG, "NotificationObject is empty");
            return;
        }

        final FSAMessage fsaMessage = notificationObject.msg;
        final int functionId = fsaMessage.getFunctionId();
        final String payload = new String(fsaMessage.getPayload(), StandardCharsets.UTF_8);
        Log.d(FsaConstant.FSA_TAG, "receiveRequest, functionId: " + functionId + ", payload: " + payload);
        if (null != mListener) {
            mListener.onReceiveRequest(functionId, payload);
        }
    }

    @Override
    public void onRequestResponse(final byte[] bytes, final FSAServiceConnectedClient fsaServiceConnectedClient, final Integer integer) {
    }

    @Override
    public void onRequestResponse(final NotificationObject notificationObject) {
    }

    @Override
    public byte[] serializeResponseToGPB() {
        return new byte[0];
    }

    @Override
    public void deserializeRequestFromGPB(final byte[] bytes) {
    }

    @Override
    public void deserializeRequestResponseFromGPB(final byte[] bytes) {
    }

    @Override
    public Integer getSerializedSize() {
        return 0;
    }

    public interface IRequestReceiveListener {
        /**
         * 请求回调
         *
         * @param functionId
         * @param payload
         */
        void onReceiveRequest(int functionId, String payload);
    }
}
