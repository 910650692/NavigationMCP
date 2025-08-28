package com.sgm.navi.hmi.mapdata.search;

import android.content.Context;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.KeyEvent;
import android.view.View;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;

import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentSearchMapDataBinding;
import com.sgm.navi.hmi.mapdata.adapter.SearchMapDataAdapter;
import com.sgm.navi.service.define.mapdata.CityDataInfo;
import com.sgm.navi.service.define.mapdata.ProvDataInfo;
import com.sgm.navi.ui.base.BaseFragment;

import java.util.ArrayList;

public class SearchMapDataFragment extends BaseFragment<FragmentSearchMapDataBinding, SearchMapDataViewModel> {
    private static final String TAG = SearchMapDataModel.class.getSimpleName();
    private SearchMapDataAdapter mSearchMapDataAdapter;
    private ArrayList<ProvDataInfo> mDataList = new ArrayList<>();

    @Override
    public int onLayoutId() {
        return R.layout.fragment_search_map_data;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        initSearchMapDataView();
        setupSearchActions();
    }

    @Override
    public void onInitData() {

    }

    @Override
    public void onResume() {
        super.onResume();
        requestFocusAndShowKeyboard();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mViewModel = null;
    }

    /**
     * 初始化离线搜索view
     */
    private void initSearchMapDataView() {
        mSearchMapDataAdapter = new SearchMapDataAdapter(getActivity());
        mBinding.rvSearchOffline.setLayoutManager(new LinearLayoutManager(getActivity()));
        mBinding.rvSearchOffline.setItemAnimator(null);
        mBinding.rvSearchOffline.setAdapter(mSearchMapDataAdapter);

        //以下是对布局进行控制，让省份占一行，城市占两列，效果相当于一个listView嵌套gridView的效果
        final GridLayoutManager manager = new GridLayoutManager(getActivity(),1);
        manager.setSpanSizeLookup(new GridLayoutManager.SpanSizeLookup() {
            @Override
            public int getSpanSize(final int position) {
                return 1;
            }
        });
        mBinding.rvSearchOffline.setLayoutManager(manager);

        mSearchMapDataAdapter.setOnChildClickListener(new SearchMapDataAdapter.OnChildClickListener() {
            @Override
            public void startAllTask(final ArrayList<Integer> adCodeList) {
                if (mViewModel != null) {
                    mViewModel.startAllTask(adCodeList);
                }
            }

            @Override
            public void pauseAllTask(final ArrayList<Integer> adCodeList) {
                if (mViewModel != null) {
                    mViewModel.pauseAllTask(adCodeList);
                }
            }

            @Override
            public void deleteAllTask(final ArrayList<Integer> cityAdCodes) {
                if (mViewModel != null) {
                    mViewModel.deleteAllTask(cityAdCodes);
                }
            }

            @Override
            public void cancelAllTask() {

            }

            @Override
            public void allDownloadTask(final ArrayList<Integer> cityDataInfos) {
                if (mViewModel != null) {
                    mViewModel.startAllTask(cityDataInfos);
                }
            }

            @Override
            public void allPauseTask(final ArrayList<Integer> cityDataInfos) {
                if (mViewModel != null) {
                    mViewModel.pauseAllTask(cityDataInfos);
                }
            }

        });
    }

    /**
     * 搜索相关事件
     */
    private void setupSearchActions() {
        mBinding.searchOfflineEditView.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(final CharSequence s, final int start, final int count, final int after) {

            }

            @Override
            public void onTextChanged(final CharSequence s, final int start, final int before, final int count) {

            }

            @Override
            public void afterTextChanged(final Editable editable) {
                if (!editable.toString().trim().isEmpty()) {
                    mBinding.editClear.setVisibility(View.VISIBLE);
                    mViewModel.searchAdCode(editable.toString().trim());
                } else {
                    // 文本为空时，清除检索到的数据
                    if (null != mSearchMapDataAdapter) {
                        mSearchMapDataAdapter.setData(mDataList);
                    }
                    mBinding.editClear.setVisibility(View.GONE);
                }
            }
        });

        mBinding.searchOfflineEditView.setOnEditorActionListener((textView, actionId, event) -> {
            if (actionId == EditorInfo.IME_ACTION_DONE ||
                    (event != null && event.getKeyCode() == KeyEvent.KEYCODE_ENTER && event.getAction() == KeyEvent.ACTION_DOWN)) {
                hideInput();
                return true;
            } else {
                return false;
            }
        });

        mBinding.clSearchMap.setOnTouchListener((v, event) -> {
            Logger.d(TAG, "clSearchMap OnTouch");
            hideInput();
            return false;
        });

    }

    /**
     * 请求焦点并显示软键盘
     */
    public void requestFocusAndShowKeyboard() {
        // 确保视图已经附加到窗口
        mBinding.searchOfflineEditView.post(() -> {
            Logger.d(TAG, "requestFocusAndShowKeyboard: ", mBinding.searchOfflineEditView.isAttachedToWindow(),
                    mBinding.searchOfflineEditView.isShown(), mBinding.searchOfflineEditView.hasFocus());
            mBinding.searchOfflineEditView.requestFocus();
            final InputMethodManager imm = (InputMethodManager) mActivity.getSystemService(Context.INPUT_METHOD_SERVICE);
            if (imm != null) {
                // 显示软键盘
                imm.showSoftInput(mBinding.searchOfflineEditView, InputMethodManager.SHOW_IMPLICIT);
            }
        });
    }

    /**
     * 隐藏软键盘
     */
    public void hideInput() {
        final InputMethodManager imm = (InputMethodManager) mActivity.getSystemService(Context.INPUT_METHOD_SERVICE);
        if (null != imm && imm.isAcceptingText()) {
            imm.hideSoftInputFromWindow(mActivity.getWindow().getDecorView().getWindowToken(), 0);
        }
    }

    /**
     * 更新搜索结果
     * @param provinceBeans
     */
    public void updateSearchResultView(final ArrayList<ProvDataInfo> provinceBeans) {
        ThreadManager.getInstance().postUi(() -> {
            mDataList.clear();
            if (provinceBeans != null && !provinceBeans.isEmpty()) {
                mBinding.noSearchOfflineView.setVisibility(View.GONE);
                mBinding.rvSearchOffline.setVisibility(View.VISIBLE);
                mDataList = provinceBeans;
                mSearchMapDataAdapter.setData(mDataList);
            } else {
                mBinding.noSearchOfflineView.setVisibility(View.VISIBLE);
                mBinding.rvSearchOffline.setVisibility(View.GONE);
            }
        });
    }

    /**
     * 更新数据列表下载进度&状态
     * @param info
     */
    public void notifySearchMapDataChangeView(final CityDataInfo info) {
        ThreadManager.getInstance().postUi(() -> {
            if (info == null) {
                Logger.e(TAG, "info is null");
                return;
            }
            if (Logger.openLog) {
                Logger.d(TAG, "notifySearchMapDataChangeView  info = ", info);
            }
            // 刷新二级列表的下载状态
            mSearchMapDataAdapter.updateChild(info.getUpperAdcode(), info.getAdcode(), info.getDownLoadInfo());
            //刷新省份城市下载状态
            mSearchMapDataAdapter.updateParent(info.getAdcode(), info.getDownLoadInfo());
        });
    }

    /**
     * 清空输入框内容
     */
    public void clearEditText() {
        mBinding.searchOfflineEditView.setText("");
        if (null != mSearchMapDataAdapter) {
            mDataList.clear();
            mSearchMapDataAdapter.setData(mDataList);
        }
    }

}
