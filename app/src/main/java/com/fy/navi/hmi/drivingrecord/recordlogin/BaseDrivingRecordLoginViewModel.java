package com.fy.navi.hmi.drivingrecord.recordlogin;

import android.app.Application;
import android.graphics.Bitmap;
import android.os.Handler;
import android.os.Looper;

import androidx.annotation.NonNull;

import com.fy.navi.hmi.drivingrecord.recordsetting.RecordSettingFragment;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

/**
 * @Description TODO
 * @Author fh
 * @date 2024/12/24
 */
public class BaseDrivingRecordLoginViewModel extends BaseViewModel<DrivingRecordLoginFragment, DrivingRecordLoginModel> {


    public BaseDrivingRecordLoginViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected DrivingRecordLoginModel initModel() {
        return new DrivingRecordLoginModel();
    }

    //返回上一页
    public Action drivingRecordBack = () -> {
        closeFragment(true);
    };

    public Action toRecordSetting = () -> {
        addFragment(new RecordSettingFragment(), null);
    };

    // 关闭当前页
    public Action toClose = () -> {
        closeFragment(true);
    };

    public void getValueByType() {
         mView.updateNoDataView(mModel.getValueByType());
    }

    public void qRCodeLoginRequest(int qrType){
        mModel.qRCodeLoginRequest(qrType);
    }

    public void updateQRCode(Bitmap bitmap) {
        mView.updateQRCode(bitmap);
    }

    public void close() {
        new Handler(Looper.getMainLooper()).post(() -> {
            closeFragment(true);
        });
    }

}
