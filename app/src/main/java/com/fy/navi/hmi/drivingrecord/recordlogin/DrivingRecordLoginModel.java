package com.fy.navi.hmi.drivingrecord.recordlogin;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.user.account.AccountRequestType;
import com.fy.navi.service.define.user.account.AccountUserInfo;
import com.fy.navi.service.define.user.usertrack.GpsTrackDepthBean;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.greendao.history.HistoryManager;
import com.fy.navi.service.logicpaket.user.account.AccountCallBack;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.service.logicpaket.user.usertrack.UserTrackCallBack;
import com.fy.navi.service.logicpaket.user.usertrack.UserTrackPackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.List;

/**
 * @Description
 * @Author fh
 * @date 2024/12/24
 */
public class DrivingRecordLoginModel extends BaseModel<DrivingRecordLoginViewModel> implements UserTrackCallBack, AccountCallBack {

    private static final String TAG = DrivingRecordLoginModel.class.getName();
    private final UserTrackPackage userTrackPackage;
    private final AccountPackage accountPackage;
    public static final int ErrorCodeLoginSuccess = 1073807360;
    private final HistoryManager historyManager;

    public DrivingRecordLoginModel() {
        userTrackPackage = UserTrackPackage.getInstance();
        accountPackage = AccountPackage.getInstance();
        historyManager = HistoryManager.getInstance();
        historyManager.init();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        userTrackPackage.registerCallBack(this);
        accountPackage.registerCallBack("DrivingRecordLoginModel",this);
    }

    public void qRCodeLoginRequest(int qrType) {
        accountPackage.qRCodeLoginRequest(qrType);
    }

    /**
     * 通过type查找其对应行程历史信息
     * @return
     */
    public int getValueByType() {
        int size = 0;
        List<History> list = historyManager.getValueByType("行程历史");
        if (list != null && !list.isEmpty()) {
            size = list.size();
        }
        return size;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    @Override
    public void notify(int eventType, int exCode) {
        // 同步事件回调
        Logger.d(TAG, "notify: eventType = " + eventType + " exCode = " + exCode);
    }

    @Override
    public void onStartGpsTrack(int n32SuccessTag, String psSavePath, String psFileName) {

    }

    @Override
    public void onCloseGpsTrack(int n32SuccessTag, String psSavePath, String psFileName, GpsTrackDepthBean depInfo) {

    }

    @Override
    public void onGpsTrackDepInfo(int n32SuccessTag, String psSavePath, String psFileName, GpsTrackDepthBean depInfo) {

    }

    @Override
    public void notifyQRCodeLogin(int errCode, int taskId, AccountUserInfo result) {
        if (result != null && result.code == 1) {
            Logger.i(TAG,"notifyQRCodeLogin AccountUserInfo = " + GsonUtils.toJson(result));
            Bitmap bitmap = BitmapFactory.decodeByteArray(result.buffer, 0, result.buffer.length);
            mViewModel.updateQRCode(bitmap);
        }
    }

    @Override
    public void notifyQRCodeLoginConfirm(int errCode, int taskId, AccountUserInfo result) {
        if (result != null && errCode == ErrorCodeLoginSuccess) {
            if (result.code == 7) {
                // 超时
                qRCodeLoginRequest(AccountRequestType.AccountTypeQRCodeLogin);
            } else if (result.code == 1) {
                Logger.i(TAG,"QRCodeLogin Success");
                mViewModel.close();
            }
        }
    }
}
