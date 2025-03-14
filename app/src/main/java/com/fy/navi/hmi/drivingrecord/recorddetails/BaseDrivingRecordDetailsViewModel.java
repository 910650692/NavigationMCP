package com.fy.navi.hmi.drivingrecord.recorddetails;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

/**
 * @Description TODO
 * @Author fh
 * @date 2024/12/24
 */
public class BaseDrivingRecordDetailsViewModel extends BaseViewModel<DrivingRecordDetailsFragment, DrivingRecordDetailsModel> {

    public BaseDrivingRecordDetailsViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected DrivingRecordDetailsModel initModel() {
        return new DrivingRecordDetailsModel();
    }

    //返回上一页
    public Action drivingRecordDetailsBack = () -> {
        closeFragment(true);
    };

    /**
     * 删除行程详情
     * @param id
     */
    public void delBehaviorData(String id) {
        mModel.delBehaviorData(id);
    }

    public void closeDrivingRecordDetailsView() {
        closeFragment(true);
    }
}
