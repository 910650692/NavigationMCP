package com.fy.navi.hmi.drivingrecord.recorddetails;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;


public class BaseDrivingRecordDetailsViewModel extends BaseViewModel<DrivingRecordDetailsFragment, DrivingRecordDetailsModel> {

    public BaseDrivingRecordDetailsViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected DrivingRecordDetailsModel initModel() {
        return new DrivingRecordDetailsModel();
    }

    //返回上一页
    public Action mDrivingRecordDetailsBack = () -> {
        LayerPackage.getInstance().addLayerItemOfUserTrackDepth(MapType.MAIN_SCREEN_MAIN_MAP, null, false);
        closeFragment(true);
    };

    //返回上一页
    public Action mDeleteRecord= () -> {
        mView.showDialog();
    };

    /**
     * 删除行程详情
     * @param id 行程详情id
     */
    public void delBehaviorData(final String id) {
        mModel.delBehaviorData(id);
    }

    /**
     * 通过数据type删除其对应info
     * @param fileName 数据文件名
     */
    public void deleteValueByFileName(final String fileName) {
        mModel.deleteValueByFileName(fileName);
    }
}
