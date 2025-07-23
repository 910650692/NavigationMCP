package com.sgm.navi.hmi.favorite;


import android.annotation.SuppressLint;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.graphics.Rect;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewTreeObserver;
import android.view.WindowManager;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.FrameLayout;
import android.widget.PopupWindow;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentFavoriteBinding;
import com.sgm.navi.hmi.favorite.adapter.FavoriteDataAdapter;
import com.sgm.navi.hmi.favorite.adapter.FrequentAddressAdapter;
import com.sgm.navi.hmi.poi.PoiDetailsFragment;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.view.SkinEditText;
import com.sgm.navi.ui.view.SkinGridLayout;
import com.sgm.navi.ui.view.SkinImageView;
import com.sgm.navi.ui.view.SkinNestedScrollView;
import com.sgm.navi.ui.view.SkinTextView;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;


public class FavoriteFragment extends BaseFragment<FragmentFavoriteBinding, FavoriteViewModel> implements ViewTreeObserver.OnScrollChangedListener {

    private final static String TAG = FavoriteFragment.class.getSimpleName();
    private ArrayList<PoiInfoEntity> mFavoriteList = new ArrayList<>();
    private ArrayList<PoiInfoEntity> mFrequentAddressList = new ArrayList<>();
    private final Map<Integer, View> mViewMap = new HashMap<>();

    private FavoriteDataAdapter mFavoriteDataAdapter;
    private FrequentAddressAdapter mFrequentAddressAdapter;

    private PopupWindow mPopupWindow;
    private SkinTextView mBtnEdit;
    private SkinTextView mBtnDelete;
    private SkinTextView mBtnDelete1;
    private PopupWindow mFrequentPopupWindow;
    private SkinTextView mRenameBtn;
    private SkinGridLayout mFreqAddressLayout;
    private View mAnchorView;
    private int mThreeScreenHeight = 0;
    private int mOldScrollY;
    private boolean mIsStopScroll;
    private boolean mIsTouchEvent;
    private long mFlingTime = 0;    //惯性滑动时间
    private final static String PATAC_ACTION_LOGIN = "patac.hmi.user.intent.action.LOGIN";

    @Override
    public int onLayoutId() {
        return R.layout.fragment_favorite;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        Logger.d(TAG, "onInitView: ");
        initFavoriteList();
        backToTop();
//        initFrequentAddress();
    }

    @Override
    public void onInitData() {
        mViewModel.getSimpleFavoriteList();
        mViewModel.getHomeFavoriteInfo();
        mViewModel.getCompanyFavoriteInfo();
        mViewModel.initView();
        initPopupWindow();
        initFrequentPopupWindow();
        initFrequentAddressList();
    }

    BroadcastReceiver mAccountReceiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            Logger.d(TAG, "onReceive: mAccountReceiver");
            ThreadManager.getInstance().postUi(() -> {
                mViewModel.refreshCollectStation();
            });
        }
    };

    @Override
    public void onInitObserver() {
        super.onInitObserver();
        final IntentFilter intentFilter = new IntentFilter();
        intentFilter.addAction(PATAC_ACTION_LOGIN);
        ContextCompat.registerReceiver(getContext(), mAccountReceiver, intentFilter, ContextCompat.RECEIVER_EXPORTED);
    }

    @Override
    public void onHiddenChanged(final boolean hidden) {
        super.onHiddenChanged(hidden);
        if (!hidden) {
            mViewModel.getSimpleFavoriteList();
            mViewModel.getHomeFavoriteInfo();
            mViewModel.getCompanyFavoriteInfo();
            initFrequentAddressList();
        }
    }

    /**
     * 初始化收藏点列表
     */
    @HookMethod(eventName = BuryConstant.EventName.AMAP_FAVORITE_LIST)
    private void initFavoriteList() {
        mFavoriteDataAdapter = new FavoriteDataAdapter();
        mFavoriteDataAdapter.setItemClickListener(new FavoriteDataAdapter.OnItemClickListener() {
            @Override
            public void onItemNaviClick(final int index) {
                final PoiInfoEntity poiInfoEntity = mFavoriteList.get(index);
                if (Logger.openLog) {
                    Logger.d(TAG, "navi click ", poiInfoEntity);
                }
                if (mViewModel != null) {
                    mViewModel.startRoute(poiInfoEntity);
                }
            }

            @Override
            public void onItemDetailClick(final int index) {
                final Bundle bundle = new Bundle();
                bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL,  mFavoriteList.get(index));
                bundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_MAP_CLICK);
                addPoiDetailsFragment(new PoiDetailsFragment(), bundle);
            }

            @Override
            public void onItemTopClick(final int index) {
                Logger.d(TAG,"on item top click");
                if (mFavoriteList != null && !mFavoriteList.isEmpty()) {
                    SettingUpdateObservable.getInstance().onUpdateSyncTime();
                    mViewModel.topFavorite(mFavoriteList.get(index), true);
                    mViewModel.getSimpleFavoriteList();
                }
            }

            @Override
            public void onItemCancelTopClick(final int index) {
                Logger.d(TAG, "on item cancel top click");
                mBinding.favoriteScroll.setEnabled(false);
                if (mFavoriteList != null && !mFavoriteList.isEmpty()) {
                    SettingUpdateObservable.getInstance().onUpdateSyncTime();
                    mViewModel.topFavorite(mFavoriteList.get(index), false);
                    mViewModel.getSimpleFavoriteList();
                    ToastUtils.Companion.getInstance().showCustomToastView("已取消置顶");
                }
                mBinding.favoriteScroll.setEnabled(true);
            }

            @Override
            public void onItemDeleteClick(final int index) {
                SettingUpdateObservable.getInstance().onUpdateSyncTime();
                if (mFavoriteList != null && !mFavoriteList.isEmpty()) {
                    mViewModel.removeFavorite(mFavoriteList.get(index));
                    mViewModel.getSimpleFavoriteList();
                    ToastUtils.Companion.getInstance().showCustomToastView("已删除");
                }
            }
        });
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getActivity());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mBinding.rvFavoriteList.setNestedScrollingEnabled(false);
        mBinding.rvFavoriteList.setLayoutManager(layoutManager);
        mBinding.rvFavoriteList.setAdapter(mFavoriteDataAdapter);
    }

    /**
     * 更新家的信息
     * @param home 家
     */
    public void updateHomeView(final PoiInfoEntity home) {
        if (home != null) {
            mBinding.favoriteHomeName.setText(home.getName());
        } else {
            mBinding.favoriteHomeName.setText(R.string.favorite_setting);
        }
    }

    /**
     * 更新公司的信息
     * @param company 公司
     */
    public void updateCompanyView(final PoiInfoEntity company) {
        if (company != null) {
            mBinding.favoriteCompanyName.setText(company.getName());
        } else {
            mBinding.favoriteCompanyName.setText(R.string.favorite_setting);
        }
    }

    /**
     * getHomeOrCompanyEditView
     * @param isHome
     * @return view
     */
    public View getHomeOrCompanyEditView(final boolean isHome) {
        if (isHome) {
            return mBinding.moreHome;
        } else {
            return mBinding.moreCompany;
        }
    }

    private int mIndex;

    /**
     * initFrequentAddressList
     */
    private void initFrequentAddressList() {
        if (getActivity() == null) {
            return;
        }
        mFreqAddressLayout = mBinding.frequentAddressContainer;
        mFreqAddressLayout.removeAllViews();

        mFrequentAddressList = mViewModel.getFavoriteAddressInfo();

        final int addressCount = mFrequentAddressList.size();
        final int maxItemPerRow = 2;
        final int margin = mViewModel.getPopupData().get("addButtonMargin");

        // 添加地址项
        for (int i = 0; i < addressCount; i++) {
            final View itemView = LayoutInflater.from(getContext()).inflate(R.layout.item_frequent_address, mFreqAddressLayout, false);
            final int withSpec = View.MeasureSpec.makeMeasureSpec(mFreqAddressLayout.getMeasuredWidth(), View.MeasureSpec.AT_MOST);
            final int heightSpec = View.MeasureSpec.makeMeasureSpec(mFreqAddressLayout.getMeasuredHeight(), View.MeasureSpec.AT_MOST);
            itemView.measure(withSpec, heightSpec);
            final SkinTextView tvName = itemView.findViewById(R.id.tv_frequent_address_text);
            final SkinImageView btnMore = itemView.findViewById(R.id.tv_frequent_address_more);
            mViewMap.put(i, itemView);

            tvName.setText(mFrequentAddressList.get(i).getName());
            final String name = mFrequentAddressList.get(i).getFavoriteInfo().getCustom_name();
            boolean isRename = false;
            if (name != null) {
                isRename = !Objects.equals(name, mFrequentAddressList.get(i).getName());
            }
            if (isRename) {
                tvName.setText(name);
            }
            final int finalI = i;
            btnMore.setOnClickListener(v -> {
                if (!isRenameStatus()) {
                    this.mAnchorView = itemView;
                    mIndex = finalI;
                    showFrequentPopupWindow(itemView);
                }
            });
            tvName.setOnClickListener(v -> {
                if (mViewModel != null) {
                    final PoiInfoEntity poiInfoEntity = mFrequentAddressList.get(finalI);
                    mViewModel.startRoute(poiInfoEntity);
                }
            });

            final SkinGridLayout.Spec rowSpec = SkinGridLayout.spec(i / maxItemPerRow, 1);
            final SkinGridLayout.Spec colSpec = SkinGridLayout.spec(i % maxItemPerRow, 1);

            final SkinGridLayout.LayoutParams params = new SkinGridLayout.LayoutParams(rowSpec, colSpec);

            params.width = itemView.getLayoutParams().width;
            params.height = itemView.getLayoutParams().height;
            params.bottomMargin = margin;
            params.rightMargin = margin;
            mFreqAddressLayout.addView(itemView, params);
        }




        // 添加按钮逻辑
        if (addressCount <= 3) {
            final View addButton = LayoutInflater.from(getContext()).inflate(R.layout.item_add_frequent_address, mFreqAddressLayout, false);
            final int withSpec = View.MeasureSpec.makeMeasureSpec(mFreqAddressLayout.getMeasuredWidth(), View.MeasureSpec.AT_MOST);
            final int heightSpec = View.MeasureSpec.makeMeasureSpec(mFreqAddressLayout.getMeasuredHeight(), View.MeasureSpec.AT_MOST);
            addButton.measure(withSpec, heightSpec);
            final int targetRow = addressCount / maxItemPerRow; // 计算应插入的行
            final int targetCol = addressCount % maxItemPerRow; // 计算应插入的列

            //BUGFIX 1058717
            if (mFrequentAddressList.size() >= 3) {
                addButton.setVisibility(View.GONE);
            } else {
                addButton.setVisibility(View.VISIBLE);
            }

            addButton.setOnClickListener(v -> {
                if (mFrequentAddressList.size() >= 3) {
                    ToastUtils.Companion.getInstance().showCustomToastView("最多添加3个常用地址");
                } else {
                    final Bundle bundle = new Bundle();
                    bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
                    bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.COMMON);
                    addFragment(new HomeCompanyFragment(), bundle);
                }
            });

            final SkinGridLayout.Spec rowSpec = SkinGridLayout.spec(targetRow, 1);
            final SkinGridLayout.Spec colSpec = SkinGridLayout.spec(targetCol, 1);

            final SkinGridLayout.LayoutParams params = new SkinGridLayout.LayoutParams(rowSpec, colSpec);
            params.width = addButton.getLayoutParams().width;
            params.height = addButton.getLayoutParams().height;
            params.rightMargin = margin;
            mFreqAddressLayout.addView(addButton, params);
        }
    }

    /**
     * 判断是否重命名状态
     * @return true/false
     */
    private boolean isRenameStatus() {
        final View view = mViewMap.get(mIndex);
        if (view != null) {
            final SkinTextView tvName = view.findViewById(R.id.tv_frequent_address_text);
            final SkinEditText etName = view.findViewById(R.id.tv_frequent_address_title);
            if (etName.getVisibility() == View.VISIBLE && etName.hasFocus()) {
                etName.setVisibility(View.GONE);
                tvName.setVisibility(View.VISIBLE);
                return true;
            }
        }
        return false;
    }

    /**
     * showRenameDialog
     * @param address
     * @param itemView
     */
    public void showRenameDialog(final PoiInfoEntity address, final View itemView) {
        final SkinTextView tvName = itemView.findViewById(R.id.tv_frequent_address_text);
        final SkinEditText etName = itemView.findViewById(R.id.tv_frequent_address_title);

        // 切换视图状态
        tvName.setVisibility(View.GONE);
        etName.setVisibility(View.VISIBLE);
        String customName = "";
        if (address.getFavoriteInfo() != null) {
            customName = address.getFavoriteInfo().getCustom_name();
        }
        etName.setText(TextUtils.isEmpty(customName) ? address.getName() : customName);
        // 焦点与键盘控制
        etName.post(() -> {
            etName.requestFocus();
            etName.setSelection(etName.getText().length()); // 光标置于末尾
            if (getActivity() != null) {
                getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_VISIBLE);
            }
        });

        // 完成编辑监听
        etName.setOnEditorActionListener((v, actionId, event) -> {
            if (actionId == EditorInfo.IME_ACTION_DONE) {
                saveRenameResult(address, tvName, etName);
                return true;
            }
            return false;
        });

        // 失去焦点时自动保存
        etName.setOnFocusChangeListener((v, hasFocus) -> {
            if (!hasFocus) {
                saveRenameResult(address, tvName, etName);
            }
        });
    }

    /**
     * saveRenameResult
     * @param address
     * @param tvName
     * @param etName
     */
    private void saveRenameResult(final PoiInfoEntity address, final SkinTextView tvName, final SkinEditText etName) {
        final String newName = etName.getText().toString().trim();
        if (!newName.isEmpty()) {
            mViewModel.modifyFavoriteData(address.getFavoriteInfo().getItemId(), newName);
            address.setName(newName);
            tvName.setText(newName);
        }

        // 恢复视图状态
        tvName.setVisibility(View.VISIBLE);
        etName.setVisibility(View.GONE);

        // 隐藏键盘
        final InputMethodManager imm = (InputMethodManager) requireContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        imm.hideSoftInputFromWindow(etName.getWindowToken(), 0);
        initFrequentAddressList();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mViewModel.onDestroy();
        getContext().unregisterReceiver(mAccountReceiver);
    }

    /**
     * 更新名称
     * @param name
     */
    public void updateFavoriteName(String name) {
        if (mIndex >= mFrequentAddressList.size()) {
            return;
        }
        final PoiInfoEntity poiInfoEntity = mFrequentAddressList.get(mIndex);
        mViewModel.modifyFavoriteData(poiInfoEntity.getFavoriteInfo().getItemId(), name);
        initFrequentAddressList();
    }

    /**
     * 打开重命名Fragment
     */
    public void openFavoriteRenameFragment(final PoiInfoEntity poiInfo) {
        final Bundle bundle = new Bundle();
        bundle.putParcelable("rename", poiInfo);
        addFragment(new FavoriteRenameFragment(), bundle);
    }

    /**
     * 更新常去地址
     * @param list 常去地址
     */
    public void updateFavoritePoiData(final ArrayList<PoiInfoEntity> list) {
        if (Logger.openLog) {
            Logger.i(TAG, "updateFavoritePoiData -> ", list);
        }
        mFrequentAddressList = list;
        ThreadManager.getInstance().postUi(() -> {
            mFrequentAddressAdapter.setData(mFrequentAddressList);
        });
    }

    /**
     * 更新收藏点列表
     * @param list 收藏点集合
     * @param type o: favorite list, 1: station list
     */
    public void updateFavoriteView(final ArrayList<PoiInfoEntity> list, final int type) {
        mFavoriteList = list;
        if (Logger.openLog) {
            Logger.i(TAG, "setFavoriteData -> ", mFavoriteList);
        }
        mFavoriteDataAdapter.setData(mFavoriteList, type);
    }

    /**
     * 初始化常去地址列表
     */
    private void initFrequentAddress() {
        mFrequentAddressAdapter = new FrequentAddressAdapter(getActivity());
        mFrequentAddressAdapter.setItemClickListener(new FrequentAddressAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(final int index) {

            }

            @Override
            public void onItemDeleteClick(final int index) {


            }

            @Override
            public void onItemAddClick(final int index) {

            }
        });
        // 创建布局管理器，指定每行的列数
        final GridLayoutManager layoutManager = new GridLayoutManager(getActivity(), 2); // 例如，每行4列
        // 添加间距装饰
        final int spacingInPixels = getResources().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.dp_20);
        mBinding.rvFrequentAddressList.addItemDecoration(new RecyclerView.ItemDecoration() {
            @Override
            public void getItemOffsets(final @NonNull Rect outRect, final @NonNull View view, final @NonNull RecyclerView parent,
                                       final @NonNull RecyclerView.State state) {
                outRect.bottom = spacingInPixels; // 设置底部间距
            }
        });
        mBinding.rvFrequentAddressList.setNestedScrollingEnabled(false);
        mBinding.rvFrequentAddressList.setLayoutManager(layoutManager);
        mBinding.rvFrequentAddressList.setAdapter(mFrequentAddressAdapter);
    }

    /**
     * initPopupWindow
     */
    private void initPopupWindow() {
        if (getActivity() == null) {
            return;
        }
        final FrameLayout decorView = (FrameLayout) getActivity().getWindow().getDecorView();

        // 创建PopupWindow的视图
        final View popupView = LayoutInflater.from(getContext()).inflate(R.layout.favorite_edit_popup, decorView, false);
        final int withSpec = View.MeasureSpec.makeMeasureSpec(decorView.getMeasuredWidth(), View.MeasureSpec.AT_MOST);
        final int heightSpec = View.MeasureSpec.makeMeasureSpec(decorView.getMeasuredHeight(), View.MeasureSpec.AT_MOST);
        popupView.measure(withSpec, heightSpec);

        // 获取PopupWindow中的按钮并设置点击事件
        mBtnEdit = popupView.findViewById(R.id.favorite_item_rename);
        mBtnDelete1 = popupView.findViewById(R.id.favorite_item_delete);

        // 创建PopupWindow
        mPopupWindow = new PopupWindow(popupView, popupView.getLayoutParams().width, popupView.getLayoutParams().height,true);
        mPopupWindow.setContentView(popupView);
        mPopupWindow.setBackgroundDrawable(null); // 使PopupWindow背景透明


        mBtnEdit.setOnClickListener(v -> {
            final Bundle bundle = new Bundle();
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
            if (mViewModel.getIsHome()) {
                bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.HOME);
            } else {
                bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.COMPANY);
            }
            addFragment(new HomeCompanyFragment(), bundle);
            mPopupWindow.dismiss();
        });

        mBtnDelete1.setOnClickListener(v -> {
            SettingUpdateObservable.getInstance().onUpdateSyncTime();
            final boolean isHome = mViewModel.getIsHome();
            mViewModel.removeFavorite(mViewModel.getHomeCompanyInfo(isHome));
            if (isHome) {
                mViewModel.updateHomeView(null);
            } else {
                mViewModel.updateCompanyView(null);
            }
            mPopupWindow.dismiss();
        });
    }

    /**
     * showPopupWindow
     * @param anchorView
     */
    public void showPopupWindow(final View anchorView) {
        ThreadManager.getInstance().postUi(() -> {
            // 显示PopupWindow在更多按钮的上方
            final int[] location = new int[2];
            anchorView.getLocationOnScreen(location);

            mBtnEdit.setText(R.string.favorite_item_edit);
            if (((mViewModel.getIsHome() && mViewModel.getHomeCompanyInfo(true) == null)) ||
                    (!mViewModel.getIsHome() && mViewModel.getHomeCompanyInfo(false) == null)) {
                mBtnDelete1.setEnabled(false);
                mBtnDelete1.setTextColor(getResources().getColor(R.color.color_black_35));
            } else {
                mBtnDelete1.setEnabled(true);
                mBtnDelete1.setTextColor(getResources().getColor(R.color.color_black_70));
            }
            final int y = mViewModel.getPopupData().get("homeOfficeY");
            mPopupWindow.showAsDropDown(anchorView, 25, y , Gravity.END);
        });
    }

    /**
     * initFrequentPopupWindow
     */
    private void initFrequentPopupWindow() {
        if (getActivity() == null) {
            return;
        }
        final FrameLayout decorView = (FrameLayout) getActivity().getWindow().getDecorView();
        // 创建PopupWindow的视图
        final View popupView = LayoutInflater.from(getContext()).inflate(R.layout.favorite_edit_popup, decorView, false);
        final int withSpec = View.MeasureSpec.makeMeasureSpec(decorView.getMeasuredWidth(), View.MeasureSpec.AT_MOST);
        final int heightSpec = View.MeasureSpec.makeMeasureSpec(decorView.getMeasuredHeight(), View.MeasureSpec.AT_MOST);
        popupView.measure(withSpec, heightSpec);
        // 获取PopupWindow中的按钮并设置点击事件
        mRenameBtn = popupView.findViewById(R.id.favorite_item_rename);
        mBtnDelete = popupView.findViewById(R.id.favorite_item_delete);

        // 创建PopupWindow
        mFrequentPopupWindow = new PopupWindow(popupView, popupView.getLayoutParams().width, popupView.getLayoutParams().height, true);
        mFrequentPopupWindow.setContentView(popupView);
        mFrequentPopupWindow.setBackgroundDrawable(null); // 使PopupWindow背景透明

        mRenameBtn.setOnClickListener(v -> {
            mViewModel.showRenameDialog(mFrequentAddressList.get(mIndex), mAnchorView);
            mFrequentPopupWindow.dismiss();
        });

        mBtnDelete.setOnClickListener(v -> {
            SettingUpdateObservable.getInstance().onUpdateSyncTime();
            ThreadManager.getInstance().postUi(() -> {
                mViewModel.removeFavorite(mFrequentAddressList.get(mIndex));
                initFrequentAddressList();
            });
            mFrequentPopupWindow.dismiss();
        });
    }

    /**
     * showFrequentPopupWindow
     * @param anchorView
     */
    public void showFrequentPopupWindow(final View anchorView) {
        final int[] location = new int[2];
        anchorView.getLocationOnScreen(location);
        final int y = mViewModel.getPopupData().get("frequentY");
        mFrequentPopupWindow.showAsDropDown(anchorView, 0, y , Gravity.END);
    }

    /**
     * backToTop
     */
    @SuppressLint("ClickableViewAccessibility")
    private void backToTop() {

        mThreeScreenHeight = ResourceUtils.Companion.getInstance().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.dp_714)  * 3;
        mBinding.favoriteScroll.setOnTouchListener(new View.OnTouchListener() {
            @Override
            public boolean onTouch(final View v, final MotionEvent event) {
                switch (event.getAction()) {
                    case MotionEvent.ACTION_DOWN:
                        mIsTouchEvent = true;
                        break;
                    case  MotionEvent.ACTION_UP:
                    case MotionEvent.ACTION_CANCEL:
                        mFlingTime = getCurrentTime();
                        mIsTouchEvent = false;
                        break;
                    default:
                        break;
                }
                return false;
            }
        });


        // 设置滚动监听
        SkinNestedScrollView favoriteScroll = mBinding.favoriteScroll;
        if (null != favoriteScroll) {
            favoriteScroll.addOnAttachStateChangeListener(new View.OnAttachStateChangeListener() {
                @Override
                public void onViewAttachedToWindow(@NonNull View v) {
                    ViewTreeObserver observer = v.getViewTreeObserver();
                    observer.addOnScrollChangedListener(FavoriteFragment.this);//attach时注册
                }

                @Override
                public void onViewDetachedFromWindow(@NonNull View v) {
                    ViewTreeObserver viewTreeObserver = v.getViewTreeObserver();
                    viewTreeObserver.removeOnScrollChangedListener(FavoriteFragment.this);//detach时反注册
                }
            });
        }

        // 按钮点击事件
        mBinding.layoutTop.setOnClickListener(v -> {
            mIsStopScroll = false;
            mBinding.favoriteScroll.smoothScrollTo(0, 0); // 平滑滚动
        });
    }

    /**
     * 只需要实现滚动监听
     */
    @Override
    public void onScrollChanged() {
        final int scrollY = mBinding.favoriteScroll.getScrollY();
        final int scrollX = mBinding.favoriteScroll.getScrollX();
        if (scrollY == 0 && mIsStopScroll && !(mIsTouchEvent || getCurrentTime() - mFlingTime < 300)) {
            mBinding.favoriteScroll.scrollTo(scrollX, mOldScrollY);
            return;
        }
        mOldScrollY = Math.max(scrollY, 0);
        mIsStopScroll = true;
        int visibility = mBinding.layoutTop.getVisibility();
        if (scrollY >= mThreeScreenHeight && (visibility == View.INVISIBLE || visibility == View.GONE)) {
            mBinding.layoutTop.setVisibility(View.VISIBLE);
            return;
        }

        if (scrollY < mThreeScreenHeight && visibility == View.VISIBLE) {
            mBinding.layoutTop.setVisibility(View.INVISIBLE);
        }
    }

    private long getCurrentTime() {
        return System.currentTimeMillis();
    }
}
