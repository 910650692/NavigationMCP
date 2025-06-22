package com.sgm.navi.vrbridge;

import android.content.Context;

import com.android.utils.log.Logger;
import com.baidu.bridge.BridgeSdk;
import com.baidu.bridge.listener.OnConnectedListener;
import com.baidu.oneos.protocol.bean.ArrivalBean;
import com.baidu.oneos.protocol.callback.PoiCallback;
import com.baidu.oneos.protocol.listener.NaviCommandListener;
import com.baidu.oneos.protocol.listener.NaviControlCommandListener;
import com.sgm.navi.vrbridge.impl.NaviCommandImpl;
import com.sgm.navi.vrbridge.impl.NaviControlCommandImpl;

/**
 * 百度VRBridge初始化入口.
 * @author tssh.
 * @version $Revision.1.0.0$
 */
public class VrBridgeManager {

    private boolean mBridgeInit = false;
    private NaviControlCommandImpl mControlCommandImpl;

    public static VrBridgeManager getInstance() {
        return VrBridgeManagerHolder.INSTANCE;
    }

    private static final class VrBridgeManagerHolder {
        private static final VrBridgeManager INSTANCE = new VrBridgeManager();
    }

    /**
     * Baidu vr bridge init.
     *
     * @param context Context.
     */
    public void init(final Context context) {
        BridgeSdk.getInstance().connect(context.getApplicationContext(), new OnConnectedListener() {
            @Override
            public void onConnected() {
                if(Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "BridgeSdk connected");
                }
                mBridgeInit = true;
                BridgeSdk.getInstance().addCapability(NaviCommandListener.class, new NaviCommandImpl());
                mControlCommandImpl = new NaviControlCommandImpl();
                BridgeSdk.getInstance().addCapability(NaviControlCommandListener.class, mControlCommandImpl);
                MapStateManager.getInstance().init();
            }

            @Override
            public void onDisconnected() {
                if(Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "BridgeSdk disconnected");
                }
                mBridgeInit = false;
                mControlCommandImpl = null;
                BridgeSdk.getInstance().removeCapability(NaviCommandListener.class);
                BridgeSdk.getInstance().removeCapability(NaviControlCommandListener.class);
            }
        });
    }

    /**
     * 保存语音搜索/导航意图指令.
     *
     * @param sessionId String 语音多轮一致性.
     * @param arrivalBean ArrivalBean，搜索参数.
     * @param poiCallback PoiCallback,语音执行结果回调.
     */
    public void saveNaviCommand(final String sessionId, final ArrivalBean arrivalBean, final PoiCallback poiCallback) {
        if (null != mControlCommandImpl) {
            mControlCommandImpl.saveNaviCommand(sessionId, arrivalBean, poiCallback);
        }
    }

    /**
     * 底图加载成功后继续执行语音指令.
     */
    public void processCommandWhenLoaded() {
        if (null != mControlCommandImpl) {
            mControlCommandImpl.processNextCommand();
        }
    }

}
