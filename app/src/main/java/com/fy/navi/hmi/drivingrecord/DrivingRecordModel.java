package com.fy.navi.hmi.drivingrecord;

import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.R;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.refix.LayerItemUserTrackDepth;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.user.account.AccountUserInfo;
import com.fy.navi.service.define.user.usertrack.GpsTrackDepthBean;
import com.fy.navi.service.define.user.usertrack.GpsTrackPointBean;
import com.fy.navi.service.greendao.history.HistoryManager;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.user.account.AccountCallBack;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.service.logicpaket.user.usertrack.UserTrackCallBack;
import com.fy.navi.service.logicpaket.user.usertrack.UserTrackPackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.List;


public class DrivingRecordModel extends BaseModel<DrivingRecordViewModel> implements UserTrackCallBack, AccountCallBack {

    private static final String TAG = DrivingRecordModel.class.getName();
    private final UserTrackPackage mUserTrackPackage;
    private final AccountPackage mAccountPackage;
    private final HistoryManager mHistoryManager;

    public DrivingRecordModel() {
        mUserTrackPackage = UserTrackPackage.getInstance();
        mAccountPackage = AccountPackage.getInstance();
        mHistoryManager = HistoryManager.getInstance();
        mHistoryManager.init();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mUserTrackPackage.registerCallBack("DrivingRecordModel",this);
        mAccountPackage.registerCallBack("DrivingRecordModel",  this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }


    /**
     * 从sdk获取行程数据列表保存到本地
     */
    public void getDrivingRecordData() {
        mUserTrackPackage.getDrivingRecordData();
    }

    /**
     * 获取行程数据列表（默认导航历史）
     */
    public void getDrivingRecordDataList() {
        mViewModel.updateDrivingRecordData(mUserTrackPackage.getDrivingRecordDataList());
    }

    /**
     * 获取巡航历史-行程数据列表（巡航历史）
     */
    public void getDrivingRecordCruiseDataList() {
        mViewModel.updateDrivingRecordData(mUserTrackPackage.getDrivingRecordCruiseDataList());
    }

    /**
     * 获取指定轨迹文件的深度信息，通过异步回调返回。
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @return 结果值
     */
    public int obtainGpsTrackDepInfo(final String psSavePath, final String psFileName) {
        return mUserTrackPackage.obtainGpsTrackDepInfo(psSavePath, psFileName);
    }


    /**
     *  根据ID删除行程信息
     *  @param id 行程ID
     *  @return 删除结果
     */
    public int delBehaviorData(final String id) {
        return mUserTrackPackage.delBehaviorData(id);
    }

    /**
     * 判断当前账号是否已登录
     */
    public void isLogin() {
        mViewModel.updateLoginTipView(mAccountPackage.isLogin());
    }

    @Override
    public void notify(final int eventType, final int exCode) {
        // 同步事件回调
        Logger.d(TAG, "notify: eventType = " + eventType + " exCode = " + exCode);
    }

    @Override
    public void notifyQRCodeLoginConfirm(final int errCode, final int taskId, final AccountUserInfo result) {
        if (result != null && result.getCode() == 1) {
            if (result.getProfileInfo() != null) {
                ThreadManager.getInstance().postUi(() -> {
                    mViewModel.updateLoginTipView(true);
                });
            }
        }
    }

    @Override
    public void onStartGpsTrack(final int n32SuccessTag, final String psSavePath, final String psFileName) {

    }

    @Override
    public void onCloseGpsTrack(final int n32SuccessTag, final String psSavePath, final String psFileName, final GpsTrackDepthBean depInfo) {

    }

    @Override
    public void onGpsTrackDepInfo(final int n32SuccessTag, final String psSavePath, final String psFileName, final GpsTrackDepthBean depInfo) {
        Logger.d(TAG, "onGpsTrackDepInfo: n32SuccessTag = " + n32SuccessTag + " psSavePath = " +
                psSavePath + " psFileName = " + psFileName + " depInfo = " + GsonUtils.toJson(depInfo));
        if (UserTrackPackage.getInstance().getIsNeedShowDialog()) {
            mViewModel.hideDialog();
            if (depInfo == null) {
                ThreadManager.getInstance().postUi(() -> {
                    ToastUtils.Companion.getInstance().showCustomToastView(
                            ResourceUtils.Companion.getInstance().getString(R.string.driving_record_download_failed));
                });
            } else {
                final LayerItemUserTrackDepth layerItemUserTrackDepth = new LayerItemUserTrackDepth();
                layerItemUserTrackDepth.setGpsTrackDepthBean(depInfo);
                LayerPackage.getInstance().addLayerItemOfUserTrackDepth(MapType.MAIN_SCREEN_MAIN_MAP, layerItemUserTrackDepth, false);
                final PreviewParams mPreviewParam = new PreviewParams();
                mPreviewParam.setMapBound(getTrackBound(depInfo.getTrackPoints()));
                mPreviewParam.setScreenLeft(1350);
                mPreviewParam.setScreenRight(500);
                mPreviewParam.setScreenTop(170);
                mPreviewParam.setScreenBottom(20);
                mPreviewParam.setbUseRect(true);
                mPreviewParam.setRouteLine(true);
                MapPackage.getInstance().showPreview(MapType.MAIN_SCREEN_MAIN_MAP,mPreviewParam);
                UserTrackPackage.getInstance().setIsNeedShowDialog(false);
            }
        }
    }

    /**
     * 获取轨迹边界
     * @param pois 轨迹点集合
     * @return 轨迹边界
     */
    public static PreviewParams.RectDouble getTrackBound(final List<GpsTrackPointBean> pois) {
        if (pois.isEmpty()) {
            return null;
        }
        double x1 = Double.MAX_VALUE;
        double y1 = Double.MAX_VALUE;
        double x2 = Double.MIN_VALUE;
        double y2 = Double.MIN_VALUE;
        for (int i = 0; i < pois.size(); i++) {
            final GpsTrackPointBean item = pois.get(i);
            x1 = Math.min(x1, item.getF64Longitude());
            y1 = Math.min(y1, item.getF64Latitude());
            x2 = Math.max(x2, item.getF64Longitude());
            y2 = Math.max(y2, item.getF64Latitude());
        }
        final PreviewParams.RectDouble rect = new PreviewParams.RectDouble(x1, x2, y2, y1);
        return rect;
    }
    /**
     * 通过数据type删除其对应info
     * @param fileName 数据文件名
     */
    public void deleteValueByFileName(final String fileName) {
        mHistoryManager.deleteValueByFileName(fileName);
    }
}
