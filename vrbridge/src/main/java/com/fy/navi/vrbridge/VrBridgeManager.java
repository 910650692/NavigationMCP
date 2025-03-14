package com.fy.navi.vrbridge;

import android.content.Context;
import android.util.Log;

import com.baidu.bridge.BridgeSdk;
import com.baidu.bridge.listener.OnConnectedListener;
import com.baidu.oneos.protocol.listener.NaviCommandListener;
import com.baidu.oneos.protocol.listener.NaviControlCommandListener;
import com.fy.navi.vrbridge.Impl.NaviCommandImpl;
import com.fy.navi.vrbridge.Impl.NaviControlCommandImpl;
import com.fy.navi.vrbridge.bean.MapState;

/**
 * 百度VRBridge初始化入口.
 */
public class VrBridgeManager {

    private boolean mBridgeInit = false;

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
    public void init(Context context) {
        BridgeSdk.getInstance().connect(context.getApplicationContext(), new OnConnectedListener() {
            @Override
            public void onConnected() {
                Log.d(IVrBridgeConstant.TAG, "BridgeSdk connected");
                mBridgeInit = true;
                BridgeSdk.getInstance().addCapability(NaviCommandListener.class, new NaviCommandImpl());
                BridgeSdk.getInstance().addCapability(NaviControlCommandListener.class, new NaviControlCommandImpl());
                MapStateManager.getInstance().init();
            }

            @Override
            public void onDisconnected() {
                Log.d(IVrBridgeConstant.TAG, "BridgeSdk disconnected");
                mBridgeInit = false;
                BridgeSdk.getInstance().removeCapability(NaviCommandListener.class);
                BridgeSdk.getInstance().removeCapability(NaviControlCommandListener.class);
            }
        });
    }
}
