package com.fy.navi.clslink;

import com.android.utils.log.Logger;
import com.cls.core.ClsLink;
import com.cls.vehicle.adas.map.v1.MpilotNavigationInformation;
import com.cls.vehicle.adas.map.v1.MpilotSDRouteList;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.navi.L2NaviBean;
import com.fy.navi.service.define.route.RouteL2Data;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.l2.L2InfoCallback;
import com.fy.navi.service.logicpaket.l2.L2Package;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
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
    public static final String TBT_URI = "cls:/adas.map/1/mpilot_navigation#MpilotNavigationInformation";
    public static final String ROUTE_URI = "cls:/adas.map/1/mpilot_navigation#MpilotSDRouteList";
    

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
            if (l2NaviBean == null) {
                Logger.w(TAG, "onL2DataCallBack: l2NaviBean null");
                return;
            }
            MpilotNavigationInformation.Builder mpilotNavigationInformation = MpilotNavigationInformation.newBuilder();
            // TODO 转换l2NaviBean为protobean
            publish(TBT_URI, Any.pack(mpilotNavigationInformation.build()));
        }
    };

    /**
     * 算路观察者
     */
    private final IRouteResultObserver mIRouteResultObserver = new IRouteResultObserver() {
        /**
         * 路线上充电站数据回调    、
         * @param routeL2Data 路线信息
         */
        @Override
        public void onL2DataCallBack(final RouteL2Data routeL2Data) {
            if (routeL2Data == null) {
                Logger.w(TAG, "onL2DataCallBack: routeL2Data null");
                return;
            }
            Logger.d(TAG, "send route data: " + routeL2Data);
//            JsonLogger.print("send route data", json);
            MpilotSDRouteList.Builder mpilotSDRouteList = MpilotSDRouteList.newBuilder();
            // TODO 转换routeL2Data为MpilotSDRouteList
            publish(ROUTE_URI, Any.pack(mpilotSDRouteList.build()));
        }
    };

    public void init() {
        if (CalibrationPackage.getInstance().adasConfigurationType() != 3) {
            Logger.i(TAG, "not CLEA Arch ADM configuration");
            return;
        }
        Logger.d(TAG, "init: ");
        ExecutorService executor = new ThreadPoolExecutor(16, Integer.MAX_VALUE, 60, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
        ClsLink.ServiceLifecycleListener listener = new ClsLink.ServiceLifecycleListener() {
            @Override
            public void onLifecycleChanged(ClsLink link, boolean ready) {
                Logger.d(TAG, "receiving link connection lifecycle event. IsReady: " + ready);
                if (ready) {
                    L2Package.getInstance().registerCallback(TAG, mL2InfoCallback);
                    RoutePackage.getInstance().registerRouteObserver(TAG, mIRouteResultObserver);
                } else {
                    L2Package.getInstance().unregisterCallback(TAG);
                    RoutePackage.getInstance().unRegisterRouteObserver(TAG);
                }
            }
        };
        mClsLink = ClsLink.create(AppContext.getInstance().getMContext(), executor, listener);
        mClsLink.connect();
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
