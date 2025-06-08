package com.fy.navi.scene.impl.navi;


import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.navi.ISceneNaviLastMile;
import com.fy.navi.scene.ui.navi.SceneNaviLastMileView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.user.msgpush.MsgPushRequestInfo;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.service.logicpaket.user.msgpush.MsgPushPackage;

import java.util.List;


public class SceneNaviLastMileImpl extends BaseSceneModel<SceneNaviLastMileView> implements ISceneNaviLastMile {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private MsgPushPackage mMsgPushPackage;
    private RoutePackage mRoutePackage;
    private boolean mIsDisplayedLastMile = false;

    public SceneNaviLastMileImpl(final SceneNaviLastMileView screenView) {
        super(screenView);
        mMsgPushPackage = MsgPushPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        mIsDisplayedLastMile = false;
    }

    @Override
    public void onSend() {
        Logger.d(TAG, "SceneNaviSendPhoneImpl click send");
        // 账号未登录不发送
        if (!AccountPackage.getInstance().isLogin()) {
            Logger.i(TAG, "SceneNaviSendPhoneImpl account is not login");
            ToastUtils.Companion.getInstance().showCustomToastView(AppCache.getInstance().getMContext().getText(R.string.navi_no_login));
            return;
        }
        // 网络未连接不发送
        if (Boolean.FALSE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
            Logger.i(TAG, "SceneNaviSendPhoneImpl network is not connected");
            ToastUtils.Companion.getInstance().showCustomToastView(AppCache.getInstance().getMContext().getText(R.string.navi_no_net));
            return;
        }
        final String value = SettingManager.getInstance().getValueByKey(
                SettingController.KEY_SETTING_IS_SEND_DESTINATION_LAST_MILE);
        // 功能未设置不发送
        if (!"1".equals(value)) {
            Logger.i(TAG, "SceneNaviSendPhoneImpl is not send destination last mile not set");
            return;
        }
        final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(mMapTypeId);
        if (!ConvertUtils.isEmpty(allPoiParamList)) {
            final RouteParam routeParam = allPoiParamList.get(allPoiParamList.size() - 1);
            final MsgPushRequestInfo msgPushRequestInfo = new MsgPushRequestInfo();
            msgPushRequestInfo.setAddress(routeParam.getAddress());
            msgPushRequestInfo.setName(routeParam.getName());
            msgPushRequestInfo.setLat(routeParam.getRealPos().getLat());
            msgPushRequestInfo.setLon(routeParam.getRealPos().getLon());
            final long resultCode = mMsgPushPackage.sendReqSendToPhone(msgPushRequestInfo);
            if (resultCode == 1) {
                ToastUtils.Companion.getInstance().showCustomToastView(AppCache.getInstance().getMContext().getText(R.string.navi_send_to_phone));
            }
            Logger.d(TAG, "SceneNaviSendPhoneImpl send result：" + resultCode + ",address：" + routeParam.getAddress());
        } else {
            Logger.e(TAG, "allPoiParamList is null");
        }
        updateSceneVisible(false);
    }

    @Override
    public void onClose() {
        Logger.d(TAG, "SceneNaviSendPhoneImpl click close");
        updateSceneVisible(false);
    }

    /**
     * 最后一公里
     *
     * @param naviEtaInfo etainfo
     **/
    public void checkLastMile(final NaviEtaInfo naviEtaInfo) {
        if (mIsDisplayedLastMile) {
            return;
        }
        if (naviEtaInfo.getAllDist() <= 1000) {
            // 账号未登录不显示
            if (!AccountPackage.getInstance().isLogin()) {
                Logger.d(TAG, "SceneNaviSendPhoneImpl account is not login");
                return;
            }
            // 网络未连接不显示
            if (Boolean.FALSE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
                Logger.d(TAG, "SceneNaviSendPhoneImpl network is not connected");
                return;
            }
            final String value = SettingManager.getInstance().getValueByKey(
                    SettingController.KEY_SETTING_IS_SEND_DESTINATION_LAST_MILE);
            // 功能未设置不显示
            if (!"1".equals(value)) {
                Logger.d(TAG, "SceneNaviSendPhoneImpl is not send destination last mile not set");
                return;
            }
            mIsDisplayedLastMile = true;
            updateSceneVisible(true);
            sendBuryPointForPopup();
        }
    }

    /**
     * @param isVisible 是否可见
     */
    private void updateSceneVisible(final boolean isVisible) {
        if (mScreenView.isVisible() == isVisible) return;
        Logger.i(MapDefaultFinalTag.NAVI_SCENE_TAG, "SceneNaviLastMileImpl", isVisible);
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState), NaviSceneId.NAVI_SCENE_LAST_MILE);
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_POPUP)
    private void sendBuryPointForPopup(){
        BuryProperty property = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, ResourceUtils.Companion.getInstance().getString(R.string.navi_send_request_info))
                .build();
        BuryPointController.getInstance().setBuryProps(property);
    }
}
