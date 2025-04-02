package com.fy.navi.hmi.drivingrecord.recorddetails;

import android.os.Bundle;
import android.view.Window;

import com.android.utils.ResourceUtils;
import com.android.utils.StringUtils;
import com.android.utils.TimeUtils;
import com.android.utils.ToastUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentDrivingRecordDetailsBinding;
import com.fy.navi.hmi.setting.SettingCheckDialog;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;


public class DrivingRecordDetailsFragment extends BaseFragment<FragmentDrivingRecordDetailsBinding, DrivingRecordDetailsViewModel> {
    private  DrivingRecordDataBean mDrivingRecordDataBean = new DrivingRecordDataBean();
    private SettingCheckDialog mDeleteDivingRecordDialog;

    @Override
    public int onLayoutId() {
        return R.layout.fragment_driving_record_details;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        ThreadManager.getInstance().postUi(() -> {
            updateRecordDetailsView(mDrivingRecordDataBean);
        });

        initDeleteDialog();

        mBinding.deleteDrivingRecord.setOnClickListener(view -> {
            mDeleteDivingRecordDialog.show();
        });
    }

    @Override
    public void onInitData() {
        final Bundle bundle = getArguments();
        if (bundle != null) {
            mDrivingRecordDataBean = bundle.getParcelable(AutoMapConstant.RecordDetailsBundleKey.BUNDLE_RECORD_DERAILS);
        }
    }

    /**
     * 更新行程详情UI数据
     * @param bean 行程详情数据
     */
    private void updateRecordDetailsView(final DrivingRecordDataBean bean) {
        if (bean != null) {
            mBinding.detailsStart.setText(StringUtils.strExcessLengthOmitted(bean.getStartPoiName(), 10));   // 设置起点
            mBinding.detailsEnd.setText(StringUtils.strExcessLengthOmitted(bean.getEndPoiName(), 15)); // 设置终点
            mBinding.detailsTime.setText(TimeUtils.convertDateFormat(bean.getStartTime())); // 该行程完成时间

            if (bean.getRideRunType() == 1) { //导航
                mBinding.detailsLevel.setText("导航");
            } else if (bean.getRideRunType() == 0) { //巡航
                mBinding.detailsLevel.setText("巡航");
            }

            mBinding.detailsGuideCount.setText(TimeUtils.getInstance().getDistanceMsg(bean.getRunDistance()));// 该行程行驶距离
            mBinding.detailsGuideTime.setText(TimeUtils.formatSecondsToHHMMSS(bean.getTimeInterval())); // 驾驶时长
            mBinding.detailsGuideSpeed.setText(String.valueOf(bean.getAverageSpeed())); // 平均速度
            mBinding.detailsGuideFastSpeed.setText(String.valueOf(bean.getMaxSpeed())); // 最快速度
        }
    }

    /**
     * 删除单条记录
     */
    public void initDeleteDialog() {
        mDeleteDivingRecordDialog = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.driving_item_delete_title))
                .setContent(ResourceUtils.Companion.getInstance().getString(R.string.driving_item_delete_message))
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.driving_item_delete))
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        if (mDrivingRecordDataBean != null) {
                            if (AccountPackage.getInstance().isLogin()) {
                                mViewModel.delBehaviorData(mDrivingRecordDataBean.getId());
                            }
                            mViewModel.deleteValueByFileName(mDrivingRecordDataBean.getTrackFileName());
                            LayerPackage.getInstance().addLayerItemOfUserTrackDepth(MapType.MAIN_SCREEN_MAIN_MAP, null, false);
                            ThreadManager.getInstance().postUi(() -> {
                                closeFragment(true);
                                ToastUtils.Companion.getInstance().showCustomToastView(
                                        ResourceUtils.Companion.getInstance().getString(R.string.driving_record_delete_success));
                            });

                        }

                    }
                }).build();
        clearBackground(mDeleteDivingRecordDialog.getWindow());
    }

    /**
     * 清除背景色
     * @param window 窗口
     */
    private void clearBackground(final Window window) {
        if (window != null) {
            window.setDimAmount(0f);
        }
    }

    @Override
    public void onResume() {
        super.onResume();
    }

    @Override
    public void onInitObserver() {
        super.onInitObserver();
    }

}
