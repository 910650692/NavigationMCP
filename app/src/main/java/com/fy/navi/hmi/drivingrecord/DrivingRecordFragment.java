package com.fy.navi.hmi.drivingrecord;

import android.view.Window;
import android.widget.RadioButton;
import android.widget.RadioGroup;

import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentDrivingRecordBinding;
import com.fy.navi.hmi.drivingrecord.adapter.DrivingRecordAdapter;
import com.fy.navi.hmi.setting.SettingCheckDialog;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.service.logicpaket.user.usertrack.UserTrackPackage;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

import java.util.ArrayList;


public class DrivingRecordFragment  extends BaseFragment<FragmentDrivingRecordBinding, DrivingRecordViewModel> {
    private DrivingRecordAdapter mDrivingRecordAdapter;
    private SettingCheckDialog mDeleteDivingRecordDialog;
    private RecordLoadingDialog mRecordLoadingDialog;
    private int mCurrentIndex = 0;

    private ArrayList<DrivingRecordDataBean> mDataList = new ArrayList<>();

    @Override
    public int onLayoutId() {
        return R.layout.fragment_driving_record;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        initDrivingRecordList();
        initDeleteDialog();
        initLoadingDialog();
    }

    @Override
    public void onInitData() {
        ThreadManager.getInstance().postDelay(() -> {
            mViewModel.isLogin(); // 判断用户当前登录状态
            mViewModel.getDrivingRecordData(); // 从云端同步数据到本地
            mViewModel.getDrivingRecordDataList(); // 从本地获取导航行程历史数据
        },0);
    }

    @Override
    public void onResume() {
        super.onResume();
    }

    @Override
    public void onInitObserver() {
        super.onInitObserver();
    }

    /**
     * 初始化行程列表
     */
    private void initDrivingRecordList() {
        mDrivingRecordAdapter = new DrivingRecordAdapter();
        mDrivingRecordAdapter.setItemClickListener(new DrivingRecordAdapter.OnItemClickListener() {

            @Override
            public void onItemClick(final int index) {
                if (Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
                    // 跳转到行程详情页
                    mCurrentIndex = index;
                    mRecordLoadingDialog.show();
                    UserTrackPackage.getInstance().setIsNeedShowDialog(true);
                    mViewModel.obtainGpsTrackDepInfo(GBLCacheFilePath.SYNC_PATH + "/403", mDataList.get(index).getTrackFileName());
                } else {
                    ToastUtils.Companion.getInstance().showCustomToastView(
                            ResourceUtils.Companion.getInstance().getString(R.string.favorite_charging_offline));
                }

            }

            @Override
            public void onItemDeleteClick(final int index) {
                mCurrentIndex = index;
                mDeleteDivingRecordDialog.show();
            }
        });
        final LinearLayoutManager manager = new LinearLayoutManager(getActivity());
        manager.setOrientation(LinearLayoutManager.VERTICAL);
        mBinding.rvDrivingRecordList.setLayoutManager(manager);
        mBinding.rvDrivingRecordList.setAdapter(mDrivingRecordAdapter);

        mBinding.drivingRecordTab1.setChecked(true);
        // 监听选择变化（可选）
        mBinding.drivingRecordTabGroup.setOnCheckedChangeListener(new RadioGroup.OnCheckedChangeListener() {
            @Override
            public void onCheckedChanged(final RadioGroup radioGroup, final int i) {
                // 获取被选中的RadioButton
                final RadioButton checkedRadioButton = getActivity().findViewById(i);
                // 执行你想要的操作，例如获取被选中的文本
                final String selectedText = checkedRadioButton.getText().toString();
                // 打印或处理选中的文本
                Logger.d("RadioGroup", "选中的文本: " + selectedText);
                if ("导航历史".equals(selectedText)) {
                    mBinding.drivingRecordTab1.setTextColor(getResources().getColor(R.color.white));
                    mBinding.drivingRecordTab2.setTextColor(getResources().getColor(R.color.main_map_limit_loading));
                    mViewModel.getDrivingRecordDataList();
                } else if ("巡航历史".equals(selectedText)) {
                    mBinding.drivingRecordTab1.setTextColor(getResources().getColor(R.color.main_map_limit_loading));
                    mBinding.drivingRecordTab2.setTextColor(getResources().getColor(R.color.white));
                    mViewModel.getDrivingRecordCruiseDataList();
                }

            }
        });
    }

    /**
     * 更新历史行程数据列表
     * @param dataList 行程数据列表
     */
    public void updateDrivingRecordView(final ArrayList<DrivingRecordDataBean> dataList) {
        ThreadManager.getInstance().postUi(() -> {
            this.mDataList = dataList;
            mDrivingRecordAdapter.setDrivingRecordList(dataList);
        });
    }

    /**
     * 刷新数据
     */
    public void getDrivingRecord() {
        ThreadManager.getInstance().postDelay(() -> {
            mViewModel.getDrivingRecordData(); // 从云端同步数据到本地
            mViewModel.getDrivingRecordDataList(); // 从本地获取导航行程历史数据
        },0);
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
                        if (AccountPackage.getInstance().isLogin()) {
                            mViewModel.delBehaviorData(mDataList.get(mCurrentIndex).getId());
                        }
                        mViewModel.deleteValueByFileName(mDataList.get(mCurrentIndex).getTrackFileName());
                        mDataList.remove(mCurrentIndex);
                        mViewModel.updateDrivingRecordData(mDataList);
                        ThreadManager.getInstance().postUi(() -> {
                            ToastUtils.Companion.getInstance().showCustomToastView(
                                    ResourceUtils.Companion.getInstance().getString(R.string.driving_record_delete_success));
                        });
                    }

                }).build();
        clearBackground(mDeleteDivingRecordDialog.getWindow());
    }

    /**
     * 加载行程历史详情dialog
     */
    public void initLoadingDialog() {
        mRecordLoadingDialog = new RecordLoadingDialog.Build(getContext())
                .setContent(ResourceUtils.Companion.getInstance().getString(R.string.driving_item_loading_message))
                .setDialogObserver(new IBaseDialogClickListener() {
                }).build();
        clearBackground(mRecordLoadingDialog.getWindow());
    }

    /**
     * 清除背景
     * @param window 窗口
     */
    private void clearBackground(final Window window) {
        if (window != null) {
            window.setDimAmount(0f);
        }
    }

    /**
     * 隐藏弹窗
     */
    public void hideDialog() {
        ThreadManager.getInstance().postUi(() -> {
            mRecordLoadingDialog.cancel();
            mViewModel.goDetailsFragment(mDataList.get(mCurrentIndex));
        });
    }
}
