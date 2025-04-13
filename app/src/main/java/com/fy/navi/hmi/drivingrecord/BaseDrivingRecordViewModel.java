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


public class BaseDrivingRecordViewModel extends BaseViewModel<DrivingRecordFragment, DrivingRecordModel> {

    public MutableLiveData<Boolean> mRecordListViewVisibility = new MutableLiveData<>(true);
    // tab下对应的无数据提示
    public MutableLiveData<Boolean> mEmptyViewVisibility = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mLoginTipVisibility = new MutableLiveData<>(false);

    public BaseDrivingRecordViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected DrivingRecordModel initModel() {
        return new DrivingRecordModel();
    }

    //返回上一页
    public Action mDrivingRecordBack = () -> {
        closeFragment(true);
    };

    //一键登录，同步里程数据
    public Action mToLogin = () -> {
        addFragment(new DrivingRecordLoginFragment(), null);
    };

    public Action mToRecordSetting = () -> {
        addFragment(new RecordSettingFragment(), null);
    };

    /**
     * 跳转到行程详情页
     * @param bean 行程详情数据
     */
    public void goDetailsFragment(final DrivingRecordDataBean bean) {
        final Bundle bundle = new Bundle();
        bundle.putParcelable(AutoMapConstant.RecordDetailsBundleKey.BUNDLE_RECORD_DERAILS, bean);
        final DrivingRecordDetailsFragment drivingRecordDetailsFragment = new DrivingRecordDetailsFragment();
        drivingRecordDetailsFragment.setArguments(bundle);
        drivingRecordDetailsFragment.onInitData();
        addFragment(drivingRecordDetailsFragment, null);
    }

    /**
     * 获取指定轨迹文件的深度信息，通过异步回调返回。
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @return 结果值
     */
    public int obtainGpsTrackDepInfo(final String psSavePath, final String psFileName) {
        return mModel.obtainGpsTrackDepInfo(psSavePath, psFileName);
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
     *  @param id 行程ID
     *  @return 结果值
     */
    public int delBehaviorData(final String id) {
        return mModel.delBehaviorData(id);
    }

    /**
     * 更新行程数据列表
     * @param dataList 行程数据列表
     */
    public void updateDrivingRecordData(final ArrayList<DrivingRecordDataBean> dataList) {
        if (dataList == null || dataList.isEmpty()) {
            mRecordListViewVisibility.setValue(false);
            mEmptyViewVisibility.setValue(true);
        } else {
            mRecordListViewVisibility.setValue(true);
            mEmptyViewVisibility.setValue(false);
            mView.updateDrivingRecordView(dataList);
        }
    }

    /**
     * 显示当前账号登录状态
     * @param isLogin 是否登录
     */
    public void updateLoginTipView(final boolean isLogin) {
        if (isLogin) {
            mLoginTipVisibility.setValue(false);
        } else {
            mLoginTipVisibility.setValue(true);
        }
    }

    /**
     * 隐藏进度框
     */
    public void hideDialog() {
        mView.hideDialog();
    }

    /**
     * 通过数据type删除其对应info
     * @param fileName 数据文件名
     */
    public void deleteValueByFileName(final String fileName) {
        mModel.deleteValueByFileName(fileName);
    }
}
