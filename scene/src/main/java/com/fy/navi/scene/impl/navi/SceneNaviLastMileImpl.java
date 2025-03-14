package com.fy.navi.scene.impl.navi;


import com.android.utils.ConvertUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.navi.ISceneNaviLastMile;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.SceneNaviLastMileView;
import com.fy.navi.service.AppContext;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.user.msgpush.MsgPushRequestInfo;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.user.msgpush.MsgPushPackage;

import java.util.List;


public class SceneNaviLastMileImpl extends BaseSceneModel<SceneNaviLastMileView> implements ISceneNaviLastMile {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private MsgPushPackage mMsgPushPackage;
    private RoutePackage mRoutePackage;
    private boolean isDisplayedLastMile = false;

    public SceneNaviLastMileImpl(SceneNaviLastMileView mScreenView) {
        super(mScreenView);
        mMsgPushPackage = MsgPushPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        isDisplayedLastMile = false;
    }

    @Override
    public void onSend() {
        Logger.d(TAG, "SceneNaviSendPhoneImpl click send");
        // TODO: 2025/1/27 此处需要判断账号是否登录，网络是否连接
        List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(mMapTypeId);
        if (!ConvertUtils.isEmpty(allPoiParamList)) {
            RouteParam routeParam = allPoiParamList.get(allPoiParamList.size() - 1);
            MsgPushRequestInfo msgPushRequestInfo = new MsgPushRequestInfo();
            msgPushRequestInfo.setAddress(routeParam.getAddress());
            msgPushRequestInfo.setName(routeParam.getName());
            msgPushRequestInfo.setLat(routeParam.getRealPos().getLat());
            msgPushRequestInfo.setLon(routeParam.getRealPos().getLon());
            long resultCode = mMsgPushPackage.sendReqSendToPhone(msgPushRequestInfo);
            if (resultCode == 1) {
                ToastUtils.Companion.getInstance().showCustomToastView(AppContext.mContext.getText(R.string.navi_send_to_phone));
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

    /***最后一公里***/
    public void checkLastMile(NaviEtaInfo naviEtaInfo) {
//        Logger.i(TAG, "NaviGuidanceViewModel isDisplayed：" + isDisplayed + ",naviSendPhoneVisibility：" + naviSendPhoneVisibility.get());
        if (isDisplayedLastMile) {
            return;
        }
        if (naviEtaInfo.allDist <= 1000) {
            isDisplayedLastMile = true;
            updateSceneVisible(true);
        }
    }

    private void updateSceneVisible(boolean isVisible) {
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ? INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneHideState), NaviSceneId.NAVI_SCENE_LAST_MILE);
    }
}
