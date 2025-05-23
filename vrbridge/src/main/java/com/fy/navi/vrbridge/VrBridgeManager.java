package com.fy.navi.vrbridge;

import android.content.Context;

import com.android.utils.log.Logger;
import com.baidu.bridge.BridgeSdk;
import com.baidu.bridge.listener.OnConnectedListener;
import com.baidu.oneos.protocol.listener.NaviCommandListener;
import com.baidu.oneos.protocol.listener.NaviControlCommandListener;
import com.fy.navi.vrbridge.impl.NaviCommandImpl;
import com.fy.navi.vrbridge.impl.NaviControlCommandImpl;

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
                Logger.d(IVrBridgeConstant.TAG, "BridgeSdk connected");
                mBridgeInit = true;
                BridgeSdk.getInstance().addCapability(NaviCommandListener.class, new NaviCommandImpl());
                mControlCommandImpl = new NaviControlCommandImpl();
                BridgeSdk.getInstance().addCapability(NaviControlCommandListener.class, mControlCommandImpl);
                MapStateManager.getInstance().init();
            }

            @Override
            public void onDisconnected() {
                Logger.d(IVrBridgeConstant.TAG, "BridgeSdk disconnected");
                mBridgeInit = false;
                mControlCommandImpl = null;
                BridgeSdk.getInstance().removeCapability(NaviCommandListener.class);
                BridgeSdk.getInstance().removeCapability(NaviControlCommandListener.class);
            }
        });
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
