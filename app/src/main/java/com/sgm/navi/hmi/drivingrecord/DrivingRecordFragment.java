package com.sgm.navi.hmi.drivingrecord;

import android.view.Window;
import android.widget.RadioButton;
import android.widget.RadioGroup;

import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentDrivingRecordBinding;
import com.sgm.navi.hmi.databinding.ItemDrivingRecordHeaderBinding;
import com.sgm.navi.hmi.drivingrecord.adapter.DrivingRecordAdapter;
import com.sgm.navi.hmi.setting.SettingCheckDialog;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.sgm.navi.service.logicpaket.user.account.AccountPackage;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;
import com.sgm.navi.ui.view.refresh.RefreshListener;

import java.util.ArrayList;


public class DrivingRecordFragment extends BaseFragment<FragmentDrivingRecordBinding, DrivingRecordViewModel> {
    private DrivingRecordAdapter mDrivingRecordAdapter;
    private SettingCheckDialog mDeleteDivingRecordDialog;
    private RecordLoadingDialog mRecordLoadingDialog;
    private int mCurrentIndex = 0;

    String selectedText = "导航历史";
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
        initRefreshListener();
        initDeleteDialog();
        initLoadingDialog();
    }

    @Override
    public void onInitData() {
        ThreadManager.getInstance().postDelay(() -> {
            mViewModel.isLogin(); // 判断用户当前登录状态
            mViewModel.getDrivingRecordData(); // 从云端同步数据到本地
            mViewModel.getDrivingRecordDataList(); // 从本地获取导航行程历史数据
        }, 0);
    }

    @Override
    public void onHiddenChanged(final boolean hidden) {
        if (!hidden) {
            if ("导航历史".equals(selectedText)) {
                mViewModel.getDrivingRecordDataList(); // 从本地获取导航行程历史数据
            } else if ("巡航历史".equals(selectedText)) {
                mViewModel.getDrivingRecordCruiseDataList(); // 从本地获取巡航行程历史数据
            }
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

    @Override
    public void onReStoreFragment() {
        super.onReStoreFragment();
        restoreFragment();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        clearDialog();
    }

    private void restoreFragment() {
        if (mViewModel.getIstDeleteDivingRecordDialog()) {
            mDeleteDivingRecordDialog.show();
        }
    }

    private void clearDialog() {
        if (mDeleteDivingRecordDialog.isShowing()) {
            mDeleteDivingRecordDialog.dismiss();
        }
        mDeleteDivingRecordDialog = null;
    }

    /**
     * 初始化行程列表
     */
    private void initDrivingRecordList() {
        mDrivingRecordAdapter = new DrivingRecordAdapter();
        mDrivingRecordAdapter.setItemClickListener(new DrivingRecordAdapter.OnItemClickListener() {

            @Override
            public void onItemClick(final int index) {
                if (mDataList.get(index).getRideRunType() == 0) {
                    mCurrentIndex = index;
                    mViewModel.goDetailsFragment(mDataList.get(index));
                    return;
                }
                if (Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
                    // 跳转到行程详情页
                    mCurrentIndex = index;
                    mRecordLoadingDialog.show();
                    mViewModel.setIsNeedShowDialog(true);
                    // 获取同步库轨迹文件
                    final String filePath = mViewModel.getFilePath(mDataList.get(index).getId());
                    Logger.d("filePath = " + filePath);
                    int x = mViewModel.obtainGpsTrackDepInfo(GBLCacheFilePath.SYNC_PATH + "/403", mDataList.get(index).getTrackFileName());
                } else {
                    ToastUtils.Companion.getInstance().showCustomToastView(
                        ResourceUtils.Companion.getInstance().getString(R.string.favorite_charging_offline));
                }

            }

            @Override
            public void onItemDeleteClick(final int index) {
                mCurrentIndex = index;
                mDeleteDivingRecordDialog.show();
                mViewModel.seIstDeleteDivingRecordDialog(true);
            }
        });
        mDrivingRecordAdapter.setItemOnCreateViewListener(new DrivingRecordAdapter.OnItemOnCreateView() {
            @Override
            public void onCreateHeaderView(ItemDrivingRecordHeaderBinding itemDrivingRecordHeaderBinding) {
                initHeader(itemDrivingRecordHeaderBinding);
            }
        });

        final LinearLayoutManager manager = new LinearLayoutManager(getActivity());
        manager.setOrientation(LinearLayoutManager.VERTICAL);
        mBinding.rvDrivingRecordList.setLayoutManager(manager);
        mBinding.rvDrivingRecordList.setAdapter(mDrivingRecordAdapter);
    }

    private void initHeader(ItemDrivingRecordHeaderBinding headerBinding) {
        headerBinding.drivingRecordTab1.setChecked(true);
        // 监听选择变化（可选）
        headerBinding.drivingRecordTabGroup.setOnCheckedChangeListener(new RadioGroup.OnCheckedChangeListener() {
            @Override
            public void onCheckedChanged(final RadioGroup radioGroup, final int i) {
                if (getActivity() == null) {
                    Logger.w("DrivingRecordFragment", "getActivity() is null");
                    return;
                }
                // 获取被选中的RadioButton
                final RadioButton checkedRadioButton = getActivity().findViewById(i);
                if (checkedRadioButton == null) {
                    Logger.w("DrivingRecordFragment", "checkedRadioButton is null for id: " + i);
                    return;
                }
                // 执行你想要的操作，例如获取被选中的文本
                selectedText = checkedRadioButton.getText().toString();
                // 打印或处理选中的文本
                Logger.d("RadioGroup", "选中的文本: " + selectedText);
                if (R.id.driving_record_tab_1 == i){
                    headerBinding.drivingRecordTab1.setTextColor(getResources().getColor(R.color.setting_white));
                    headerBinding.drivingRecordTab2.setTextColor(getResources().getColor(R.color.setting_bg_tab_text_unselect));
                    mViewModel.getDrivingRecordDataList();
                } else if (R.id.driving_record_tab_2 == i) {
                    headerBinding.drivingRecordTab1.setTextColor(getResources().getColor(R.color.setting_bg_tab_text_unselect));
                    headerBinding.drivingRecordTab2.setTextColor(getResources().getColor(R.color.setting_white));
                    mViewModel.getDrivingRecordCruiseDataList();
                }
            }
        });
    }

    /**
     * 更新历史行程数据列表
     *
     * @param dataList 行程数据列表
     */
    public void updateDrivingRecordView(final ArrayList<DrivingRecordDataBean> dataList) {
        ThreadManager.getInstance().postUi(() -> {
            this.mDataList = dataList;
            mDrivingRecordAdapter.setDrivingRecordList(dataList);
        });
    }

    /**
     * 设置刷新监听
     */
    private void initRefreshListener() {
        mBinding.pullRefreshLayout.setRefreshTips(getString(R.string.driving_record_refresh_pull_tips));
        mBinding.pullRefreshLayout.setRefreshListener(new RefreshListener() {
            @Override
            public void refresh() {
                updateDrivingRecordData();
                if (mBinding != null) {
                    mBinding.pullRefreshLayout.finishRefresh();
                }
            }

            @Override
            public void loadMore() {
                updateDrivingRecordData();
                if (mBinding != null) {
                    mBinding.pullRefreshLayout.finishLoadMore();
                }
            }
        });
    }

    private void updateDrivingRecordData() {
        ItemDrivingRecordHeaderBinding itemHeaderBinding = mDrivingRecordAdapter.getItemHeaderBinding();
        if (itemHeaderBinding == null) {
            return;
        }
        int checkedRadioButtonId = itemHeaderBinding.drivingRecordTabGroup.getCheckedRadioButtonId();
        if (R.id.driving_record_tab_1 == checkedRadioButtonId) {
            mViewModel.getDrivingRecordDataList(); // 从本地获取导航行程历史数据
        } else if (R.id.driving_record_tab_2 == checkedRadioButtonId) {
            mViewModel.getDrivingRecordCruiseDataList(); // 从本地获取巡航行程历史数据
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
                    mViewModel.seIstDeleteDivingRecordDialog(false);
                    if (AccountPackage.getInstance().isLogin()) {
                        mViewModel.delBehaviorData(mDataList.get(mCurrentIndex).getId());
                    }
                    mViewModel.deleteValueByFileName(mDataList.get(mCurrentIndex).getId(),
                        mDataList.get(mCurrentIndex).getRideRunType());
                    mDataList.remove(mCurrentIndex);
                    mViewModel.updateDrivingRecordData(mDataList);
                    ThreadManager.getInstance().postUi(() -> {
                        ToastUtils.Companion.getInstance().showCustomToastView(
                            ResourceUtils.Companion.getInstance().getString(R.string.driving_record_delete_success));
                    });
                }

                @Override
                public void onCancelClick() {
                    mViewModel.seIstDeleteDivingRecordDialog(false);
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
     *
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
