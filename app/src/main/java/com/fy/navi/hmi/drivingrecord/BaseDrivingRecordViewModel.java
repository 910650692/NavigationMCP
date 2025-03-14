package com.fy.navi.hmi.drivingrecord;

import android.app.Application;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.fy.navi.hmi.drivingrecord.recorddetails.DrivingRecordDetailsFragment;
import com.fy.navi.hmi.drivingrecord.recordlogin.DrivingRecordLoginFragment;
import com.fy.navi.hmi.drivingrecord.recordsetting.RecordSettingFragment;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author fh
 * @date 2024/12/24
 */
public class BaseDrivingRecordViewModel extends BaseViewModel<DrivingRecordFragment, DrivingRecordModel> {

    public MutableLiveData<Boolean> recordListViewVisibility = new MutableLiveData<>(true);
    // tab下对应的无数据提示
    public MutableLiveData<Boolean> emptyViewVisibility  = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> loginTipVisibility  = new MutableLiveData<>(false);

    public BaseDrivingRecordViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected DrivingRecordModel initModel() {
        return new DrivingRecordModel();
    }

    //返回上一页
    public Action drivingRecordBack = () -> {
        closeFragment(true);
    };

    //一键登录，同步里程数据
    public Action toLogin = () -> {
        addFragment(new DrivingRecordLoginFragment(), null);
    };

    public Action toRecordSetting = () -> {
        addFragment(new RecordSettingFragment(), null);
    };

    /**
     * 跳转到行程详情页
     */
    public void goDetailsFragment(DrivingRecordDataBean bean) {
        Bundle bundle = new Bundle();
        bundle.putParcelable(AutoMapConstant.RecordDetailsBundleKey.BUNDLE_RECORD_DERAILS, bean);
        DrivingRecordDetailsFragment drivingRecordDetailsFragment = new DrivingRecordDetailsFragment();
        drivingRecordDetailsFragment.setArguments(bundle);
        drivingRecordDetailsFragment.onInitData();
        addFragment(drivingRecordDetailsFragment, null);
    }

    /**
     * 从sdk获取行程数据列表保存到本地
     */
    public void getDrivingRecordData() {
        mModel.getDrivingRecordData();
    }

    /**
     * 获取全部行程列表（导航）
     */
    public void getDrivingRecordDataList() {
        mModel.getDrivingRecordDataList();
    }

    /**
     * 获取全部行程列表（巡航）
     */
    public void getDrivingRecordCruiseDataList() {
        mModel.getDrivingRecordCruiseDataList();
    }

    /**
     * 判断当前账号是否已登录
     */
    public void isLogin() {
        mModel.isLogin();
    }

    /**
     *  根据ID删除行程信息
     */
    public int delBehaviorData(String id) {
        return mModel.delBehaviorData(id);
    }

    public void updateDrivingRecordData(ArrayList<DrivingRecordDataBean> dataList) {
        if (dataList == null || dataList.isEmpty()) {
            recordListViewVisibility.setValue(false);
            emptyViewVisibility.setValue(true);
        } else {
            recordListViewVisibility.setValue(true);
            emptyViewVisibility.setValue(false);
            mView.updateDrivingRecordView(dataList);
        }
    }

    public void updateDrivingRecordData() {
        mView.getDrivingRecord();
    }

    /**
     * 显示当前账号登录状态
     */
    public void updateLoginTipView(boolean isLogin) {
        if (isLogin) {
            loginTipVisibility.setValue(false);
        } else {
            loginTipVisibility.setValue(true);
        }
    }

}
