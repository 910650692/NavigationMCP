package com.fy.navi.hmi.mapdata.search;

import android.annotation.SuppressLint;
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
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentSearchMapDataBinding;
import com.fy.navi.hmi.mapdata.adapter.BaseSearchMapDataAdapter;
import com.fy.navi.hmi.mapdata.adapter.SearchMapDataAdapter;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.ui.base.BaseFragment;

import java.util.ArrayList;
import java.util.List;

public class SearchMapDataFragment extends BaseFragment<FragmentSearchMapDataBinding, SearchMapDataViewModel> {
    private static final String TAG = SearchMapDataModel.class.getSimpleName();
    private SearchMapDataAdapter mSearchMapDataAdapter;
    private List<BaseSearchMapDataAdapter.DataTree<ProvDataInfo, String>> mDataList = new ArrayList<>();

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
        requestFocusAndShowKeyboard();
    }

    @Override
    public void onInitData() {

    }

    /**
     * 初始化离线搜索view
     */
    private void initSearchMapDataView() {
        mSearchMapDataAdapter = new SearchMapDataAdapter(getActivity());
        mBinding.rvSearchOffline.setLayoutManager(new LinearLayoutManager(getActivity()));
        mBinding.rvSearchOffline.setItemAnimator(null);
        mSearchMapDataAdapter.setData(mDataList);
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

        mSearchMapDataAdapter.setOfflineItemListener(new SearchMapDataAdapter.OfflineItemListener() {
            @Override
            public void startAllTask(final ArrayList<Integer> adCodeList) {
                ThreadManager.getInstance().postDelay(() -> {
                    if (mViewModel != null) {
                        mViewModel.startAllTask(adCodeList);
                    }
                }, 0);
            }

            @Override
            public void pauseAllTask(final ArrayList<Integer> adCodeList) {
                ThreadManager.getInstance().postDelay(() -> {
                    if (mViewModel != null) {
                        mViewModel.pauseAllTask(adCodeList);
                    }
                }, 0);
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
                        mDataList.clear();
                        mSearchMapDataAdapter.notifyNewData(mDataList);
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

    }

    /**
     * 请求焦点并显示软键盘
     */
    public void requestFocusAndShowKeyboard() {
        // 确保视图已经附加到窗口
        mBinding.searchOfflineEditView.post(() -> {
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
        if (null != imm) {
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

                for (int i = 0; i < provinceBeans.size(); i++) {
                    final List<CityDataInfo> city = provinceBeans.get(i).getCityInfoList();
                    mDataList.add(new BaseSearchMapDataAdapter.DataTree<>(provinceBeans.get(i), city));
                }
                mSearchMapDataAdapter.notifyNewData(mDataList);
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
    @SuppressLint("NotifyDataSetChanged")
    public void notifySearchMapDataChangeView(final ProvDataInfo info) {
        ThreadManager.getInstance().postUi(() -> {
            Logger.d(TAG,"notifySearchMapDataChangeView  info = " + GsonUtils.toJson(info));
            final CityDataInfo cityDataInfo = info.getCityInfoList().get(0);
            for (int i = 0; i < mSearchMapDataAdapter.getData().size(); i++) {

                if ( mSearchMapDataAdapter.getGroupItem(i).getAdcode() == cityDataInfo.getAdcode()) {
                    // 刷新一级列表下的子级列表下载状态
                    mSearchMapDataAdapter.getData().set(i, new BaseSearchMapDataAdapter.DataTree<>(info, info.getCityInfoList()));
                    mSearchMapDataAdapter.notifyDataSetChanged();
                    break;
                } else {
                    // 刷新二级列表下的子级列表下载状态
                    for (int j = 0; j < mSearchMapDataAdapter.getSubItem(i).size(); j++) {
                        if (mSearchMapDataAdapter.getSubItem(i).get(j).getAdcode() == cityDataInfo.getAdcode()) {
                            mSearchMapDataAdapter.getSubItem(i).set(j, cityDataInfo);
                            mSearchMapDataAdapter.notifyDataSetChanged();
                            break;
                        }
                    }
                }
            }

        });
    }

    /**
     * 清空输入框内容
     */
    public void clearEditText() {
        mBinding.searchOfflineEditView.setText("");
        if (null != mSearchMapDataAdapter) {
            mDataList.clear();
            mSearchMapDataAdapter.notifyNewData(mDataList);
        }
    }

}
