package com.fy.navi.hmi.drivingrecord.recorddetails;

import android.os.Bundle;
import android.view.Window;

import com.android.utils.ResourceUtils;
import com.android.utils.StringUtils;
import com.android.utils.TimeUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentDrivingRecordDetailsBinding;
import com.fy.navi.hmi.setting.SettingCheckDialog;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

/**
 * @Description 用户行驶记录详情页
 * @Author fh
 * @date 2024/03/02
 */
public class DrivingRecordDetailsFragment extends BaseFragment<FragmentDrivingRecordDetailsBinding, DrivingRecordDetailsViewModel> {
    private  DrivingRecordDataBean bean = new DrivingRecordDataBean();
    private SettingCheckDialog deleteDivingRecordDialog;

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
            updateRecordDetailsView(bean);
        });

        initDeleteDialog();

        mBinding.deleteDrivingRecord.setOnClickListener(view -> {
            deleteDivingRecordDialog.show();
        });
    }

    @Override
    public void onInitData() {
        Bundle bundle = getArguments();
        if (bundle != null) {
            bean = bundle.getParcelable(AutoMapConstant.RecordDetailsBundleKey.BUNDLE_RECORD_DERAILS);
        }
    }

    // 更新行程详情UI数据
    private void updateRecordDetailsView(DrivingRecordDataBean bean) {
        if (bean != null) {
            mBinding.detailsStart.setText(StringUtils.strExcessLengthOmitted(bean.getStartPoiName(), 10));   // 设置起点
            mBinding.detailsEnd.setText(StringUtils.strExcessLengthOmitted(bean.getEndPoiName(), 15)); // 设置终点
            mBinding.detailsTime.setText(TimeUtils.convertDateFormat(bean.getEndTime())); // 该行程完成时间

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
        deleteDivingRecordDialog = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.driving_item_delete_title))
                .setContent(ResourceUtils.Companion.getInstance().getString(R.string.driving_item_delete_message))
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.driving_item_delete))
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        if (bean != null) {
                            mViewModel.delBehaviorData(bean.getId());

                            ThreadManager.getInstance().postUi(() -> {
                                mViewModel.closeDrivingRecordDetailsView();
                            });

                        }

                    }
                    @Override
                    public void onCancelClick() {

                    }
                }).build();
        clearBackground(deleteDivingRecordDialog.getWindow());
    }

    private void clearBackground(Window window) {
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
