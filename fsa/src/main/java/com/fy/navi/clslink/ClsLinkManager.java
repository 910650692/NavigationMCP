package com.fy.navi.clslink;

import android.util.Log;

import com.cls.core.ClsLink;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.navi.L2NaviBean;
import com.fy.navi.service.logicpaket.l2.L2InfoCallback;
import com.fy.navi.service.logicpaket.l2.L2Package;
import com.google.protobuf.Any;
import com.sgm.cls.sdk.uprotocol.cloudevent.datamodel.CloudEventAttributes;
import com.sgm.cls.sdk.uprotocol.cloudevent.factory.CloudEventFactory;
import com.sgm.cls.sdk.uprotocol.uri.datamodel.Authority;
import com.sgm.cls.sdk.uprotocol.uri.datamodel.Entity;
import com.sgm.cls.sdk.uprotocol.uri.factory.ClsUriFactory;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import io.cloudevents.CloudEvent;

public class ClsLinkManager {
    private static final String TAG = "ClsLinkManager";
    public static final String VERSION = "1";
    private ClsLink mClsLink;

    public static ClsLinkManager getInstance() {
        return ClsLinkManager.SingleHolder.INSTANCE;
    }

    private final static class SingleHolder {
        private static final ClsLinkManager INSTANCE = new ClsLinkManager();
    }

    private ClsLinkManager() {
    }

    private final L2InfoCallback mL2InfoCallback = new L2InfoCallback() {
        @Override
        public void onSdTbtDataChange(L2NaviBean l2NaviBean) {
            // TODO 转换l2NaviBean为protobean并发送
        }
    };

    public void init() {
        Log.d(TAG, "init: ");
        ExecutorService executor = new ThreadPoolExecutor(16, Integer.MAX_VALUE, 60, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
        ClsLink.ServiceLifecycleListener listener = new ClsLink.ServiceLifecycleListener() {
            @Override
            public void onLifecycleChanged(ClsLink link, boolean ready) {
                Log.d(TAG, "receiving link connection lifecycle event. IsReady: " + ready);
                if (ready) {
                    // TODO
                }
            }
        };
        mClsLink = ClsLink.create(AppContext.getInstance().getMContext(), executor, listener);
        mClsLink.connect();

        L2Package.getInstance().registerCallback(TAG, mL2InfoCallback);
    }

    private void publish(String uri, Any protoPayload) {
        CloudEvent cloudEvent = CloudEventFactory.publish(uri, protoPayload, CloudEventAttributes.empty());
        mClsLink.publish(cloudEvent);
    }

    private void registerTopic(String uri, String methodName, ClsLink.EventListener listener) {
        Entity entity = new Entity(uri, VERSION);
        String topic = ClsUriFactory.buildMethodUri(Authority.local(), entity, methodName);
        mClsLink.registerEventListener(topic, listener);
    }
}
