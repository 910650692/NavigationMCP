package com.sgm.navi.hmi.drivingrecord.recordlogin;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;

import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.R;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.user.account.AccountUserInfo;
import com.sgm.navi.service.define.user.account.QRCodeType;
import com.sgm.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.sgm.navi.service.define.user.usertrack.GpsTrackDepthBean;
import com.sgm.navi.service.greendao.history.History;
import com.sgm.navi.service.greendao.history.HistoryManager;
import com.sgm.navi.service.logicpaket.user.account.AccountCallBack;
import com.sgm.navi.service.logicpaket.user.account.AccountPackage;
import com.sgm.navi.service.logicpaket.user.usertrack.UserTrackCallBack;
import com.sgm.navi.service.logicpaket.user.usertrack.UserTrackPackage;
import com.sgm.navi.ui.base.BaseModel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class DrivingRecordLoginModel extends BaseModel<DrivingRecordLoginViewModel> implements UserTrackCallBack, AccountCallBack {

    private static final String TAG = DrivingRecordLoginModel.class.getName();
    private final UserTrackPackage mUserTrackPackage;
    private final AccountPackage mAccountPackage;
    public static final int ERROR_CODE_LOGIN_SUCCESS = 1073807360;
    private final HistoryManager mHistoryManager;
    private ArrayList<DrivingRecordDataBean> mNeedSync;

    public DrivingRecordLoginModel() {
        mUserTrackPackage = UserTrackPackage.getInstance();
        mAccountPackage = AccountPackage.getInstance();
        mHistoryManager = HistoryManager.getInstance();
        mHistoryManager.init();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mUserTrackPackage.registerCallBack("DrivingRecordLoginModel",this);
        mAccountPackage.registerCallBack("DrivingRecordLoginModel",this);
        NetWorkUtils.Companion.getInstance().registerNetworkObserver(mNetworkObserver);
    }

    /**
     * 初始化数据
     */
    public void initView() {
        final ArrayList<DrivingRecordDataBean> drivingRecordDataList = mUserTrackPackage.getDrivingRecordDataFromDB();
        mViewModel.setLoginVisible(drivingRecordDataList != null);
    }


    /**
     * 请求二维码登录
     * @param qrType 二维码类型
     */
    public void qrCodeLoginRequest(final int qrType) {
        mAccountPackage.qrcodeloginrequest(qrType);
    }

    /**
     * 通过type查找其对应行程历史信息
     * @return size
     */
    public int getValueByType() {
        int size = 0;
        final List<History> list = mHistoryManager.getValueByType(2);
        if (list != null && !list.isEmpty()) {
            size = list.size();
        }
        return size;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        NetWorkUtils.Companion.getInstance().unRegisterNetworkObserver(mNetworkObserver);
        mAccountPackage.unRegisterCallBack("DrivingRecordLoginModel");
    }

    @Override
    public void notify(final int eventType, final int exCode) {
        // 同步事件回调
        Logger.d(TAG, "notify: eventType = " + eventType + " exCode = " + exCode);
    }

    @Override
    public void onStartGpsTrack(final int n32SuccessTag, final String psSavePath, final String psFileName) {

    }

    @Override
    public void onCloseGpsTrack(final int n32SuccessTag, final String psSavePath, final String psFileName, final GpsTrackDepthBean depInfo) {

    }

    @Override
    public void onGpsTrackDepInfo(final int n32SuccessTag, final String psSavePath, final String psFileName, final GpsTrackDepthBean depInfo) {
        if (!mUserTrackPackage.getIsNeedShowDialog()) {
            UserTrackPackage.getInstance().saveGpsTrack(psSavePath, psFileName, depInfo);
            UserTrackPackage.getInstance().geoSearch(8, new GeoPoint(depInfo.getTrackPoints().get(0).getF64Longitude(),
                    depInfo.getTrackPoints().get(0).getF64Latitude()));
        }
    }

    @Override
    public void notifyQRCodeLogin(final int errCode, final int taskId, final AccountUserInfo result) {
        if (result != null && result.getCode() == 1) {
            Logger.i(TAG,"notifyQRCodeLogin AccountUserInfo = " + GsonUtils.toJson(result));
            final Bitmap bitmap = BitmapFactory.decodeByteArray(result.getBuffer(), 0, result.getBuffer().length);
            mViewModel.stopAnimation();
            mViewModel.updateLoadingVisible(false, false, true);
            mViewModel.updateQRCode(bitmap);
        } else {
            mViewModel.updateLoadingVisible(false, true, false);
            mViewModel.updateQRCode(null);
        }
    }

    @Override
    public void notifyQRCodeLoginConfirm(final int errCode, final int taskId, final AccountUserInfo result) {
        if (result != null && errCode == ERROR_CODE_LOGIN_SUCCESS) {
            if (result.getCode() == 7) {
                // 超时
                mViewModel.startAnimation();
                qrCodeLoginRequest(QRCodeType.QR_CODE_TYPE_DEFAULT);
            } else if (result.getCode() == 1) {
                Logger.i(TAG,"QRCodeLogin Success");

                final ArrayList<DrivingRecordDataBean> localData = mUserTrackPackage.getDrivingRecordDataFromDB();
                final ArrayList<DrivingRecordDataBean> cloudData = mUserTrackPackage.getDrivingRecordDataFromSdk();

                mNeedSync = findUnsyncedLocalData(localData, cloudData);
                if (!mNeedSync.isEmpty()) {
                    if (mViewModel.isRecordLoginFragment()) {
                        mViewModel.showMergeDivingRecordDialog();
                    }
                } else {
                    mViewModel.closeFragment(true);
                }
            }
        }
    }

    /**
     * 识别需要从本地同步到云端的数据
     * @param localList 本地数据列表
     * @param cloudList 云端数据列表
     * @return 需要同步到云端的本地数据列表
     */
    public ArrayList<DrivingRecordDataBean> findUnsyncedLocalData(final ArrayList<DrivingRecordDataBean> localList,
                                                                  final ArrayList<DrivingRecordDataBean> cloudList) {
        final ArrayList<DrivingRecordDataBean> needUploadList = new ArrayList<>();

        // 空值安全处理
        if (localList == null || localList.isEmpty()) {
            return needUploadList;
        }
        if (cloudList == null) {
            return localList;
        }

        // 构建云端数据索引（复合键 -> 出现次数）
        final Map<String, Integer> cloudCountMap = new HashMap<>();
        for (DrivingRecordDataBean cloudBean : cloudList) {
            if (cloudBean == null)  {
                continue;
            }
            final String key = createCompositeKey(cloudBean);
            cloudCountMap.put(key, cloudCountMap.getOrDefault(key, 0) + 1);
        }

        // 遍历本地数据
        final Map<String, Integer> tempCountMap = new HashMap<>();
        for (DrivingRecordDataBean localBean : localList) {
            if (localBean == null) {
                continue;
            }

            final String localKey = createCompositeKey(localBean);

            // 统计当前本地键出现次数
            final int localCount = tempCountMap.getOrDefault(localKey, 0) + 1;
            tempCountMap.put(localKey, localCount);

            // 获取云端对应键的计数
            final int cloudCount = cloudCountMap.getOrDefault(localKey, 0);

            // 如果本地出现次数超过云端，则记录需要同步
            if (localCount > cloudCount) {
                needUploadList.add(localBean);
            }
        }

        return needUploadList;
    }


    /**
     * 创建复合键（处理空值）
     * @param bean 数据对象
     * @return 复合键字符串
     */
    private String createCompositeKey(final DrivingRecordDataBean bean) {
        return Objects.toString(bean.getTrackFileName(), "null")
                + "|" + bean.getRideRunType();
    }

    /**
     * 获取需要同步行程历史数据列表
     * @return 需要同步行程历史数据列表
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataList() {
        return mNeedSync;
    }

    private final NetWorkUtils.NetworkObserver mNetworkObserver = new NetWorkUtils.NetworkObserver() {
        @Override
        public void onNetConnectSuccess() {

        }

        @Override
        public void onNetDisConnect() {
            ToastUtils.Companion.getInstance().showCustomToastView(
                    ResourceUtils.Companion.getInstance().getString(R.string.setting_qr_code_load_offline_toast));
            mViewModel.stopAnimation();
            mViewModel.updateLoadingVisible(false, true, false);
        }

        @Override
        public void onNetUnavailable() {

        }

        @Override
        public void onNetBlockedStatusChanged() {

        }

        @Override
        public void onNetLosing() {

        }

        @Override
        public void onNetLinkPropertiesChanged() {

        }
    };
}
