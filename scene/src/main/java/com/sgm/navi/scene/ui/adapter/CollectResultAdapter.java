
package com.sgm.navi.scene.ui.adapter;


import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.CollectResultItemBinding;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.sgm.navi.service.logicpaket.user.account.AccountPackage;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;

import java.util.ArrayList;
import java.util.List;

public class CollectResultAdapter extends RecyclerView.Adapter<CollectResultAdapter.ResultHolder> {
    private final SearchPackage mSearchPackage;
    private final List<PoiInfoEntity> mPoiEntities;
    private OnItemClickListener mOnItemClickListener;
    //0 普通收藏 1 常用地址 3 收到的点
    private int mCollectionType = 0;
    private int mHomeCompanyType = -1;


    public void setOnItemClickListener(final OnItemClickListener listener) {
        mOnItemClickListener = listener;
    }

    /**
     * @param collectionType 0 普通收藏 1 常用地址 3 收到的点
     */
    public void setCollectionType(final int collectionType) {
        this.mCollectionType = collectionType;
        notifyList(mPoiEntities);
    }

    /**
     * @param homeCompanyType 0 普通收藏 1 家 2 公司 3 常用地址 4 途径点
     */
    public void setHomeCompanyType(final int homeCompanyType) {
        this.mHomeCompanyType = homeCompanyType;
    }


    public CollectResultAdapter() {
        mSearchPackage = SearchPackage.getInstance();
        this.mPoiEntities = new ArrayList<>();
    }

    /**
     * 更新数据并刷新RecycleView
     * @param searchResultEntity 源数据
     */
    public void notifyList(final List<PoiInfoEntity> searchResultEntity) {
        final int oldSize = mPoiEntities.size();
        final int newSize = searchResultEntity.size();

        mPoiEntities.clear();
        mPoiEntities.addAll(searchResultEntity);

        if (oldSize == 0 && newSize > 0) {
            notifyItemRangeInserted(0, newSize);
        } else if (oldSize > 0 && newSize == 0) {
            notifyItemRangeRemoved(0, oldSize);
        } else if (oldSize > 0) {
            notifyItemRangeChanged(0, Math.min(oldSize, newSize));
            if (newSize > oldSize) {
                notifyItemRangeInserted(oldSize, newSize - oldSize);
            } else if (newSize < oldSize) {
                notifyItemRangeRemoved(newSize, oldSize - newSize);
            }
        }
    }

    @NonNull
    @Override
    public ResultHolder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final CollectResultItemBinding adapterSearchResultItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.collect_result_item, parent, false);
        return new ResultHolder(adapterSearchResultItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull final ResultHolder holder, final int position) {
        holder.mResultItemBinding.setPoiBean(mPoiEntities.get(position));
        holder.mResultItemBinding.poiIcon.setVisibility(View.GONE);
        holder.mResultItemBinding.poiNum.setVisibility(View.VISIBLE);
        holder.mResultItemBinding.setLayoutPosition(String.valueOf(position + 1));
        if (!ConvertUtils.isEmpty(mPoiEntities.get(position).getFavoriteInfo())
                && mPoiEntities.get(position).getFavoriteInfo().getTop_time() != 0) {
            holder.mResultItemBinding.itemFavoriteTopTag.setVisibility(View.GONE);
//            holder.mResultItemBinding.itemFavoriteDistance.setVisibility(View.VISIBLE);
//            holder.mResultItemBinding.itemFavoriteLine.setVisibility(View.VISIBLE);
            holder.mResultItemBinding.swipeMenuLayout.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.color.navi_color_8E97B9_20));
            holder.mResultItemBinding.stvTop.setText(ResourceUtils.Companion.getInstance().getString(R.string.dsc_cancel));
            holder.mResultItemBinding.imgTop.setImageResource(R.drawable.img_untop_58);
        } else {
            holder.mResultItemBinding.itemFavoriteTopTag.setVisibility(View.GONE);
            holder.mResultItemBinding.swipeMenuLayout.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.color.transparent));
            holder.mResultItemBinding.stvTop.setText(ResourceUtils.Companion.getInstance().getString(R.string.favorite_item_top));
            holder.mResultItemBinding.imgTop.setImageResource(R.drawable.img_top_pinned);
        }
        // 专属充电站无需置顶
        if(!ConvertUtils.isEmpty(mPoiEntities.get(position).getOperatorId())){
            holder.mResultItemBinding.sllTop.setVisibility(View.INVISIBLE);
        }else{
            holder.mResultItemBinding.sllTop.setVisibility(View.VISIBLE);
        }
        //根据UE，收藏点不需要显示距离数据
        holder.mResultItemBinding.itemFavoriteDistance.setVisibility(View.GONE);
        holder.mResultItemBinding.itemFavoriteLine.setVisibility(View.GONE);
        holder.mResultItemBinding.textNavi.setText(R.string.st_go_here);
        holder.mResultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance()
                .getDrawable(R.drawable.img_basic_ic_navi));
        if (mSearchPackage.isAlongWaySearch()) {
            if (RoutePackage.getInstance().isBelongRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, mPoiEntities.get(position))) {
                holder.mResultItemBinding.textNavi.setText(R.string.route_service_list_item_added);
                holder.mResultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance()
                        .getDrawable(R.drawable.img_route_search_added));
            } else {
                holder.mResultItemBinding.textNavi.setText(R.string.st_along_way_point);
                holder.mResultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance()
                        .getDrawable(R.drawable.img_addq_58));
            }
            if (mCollectionType == AutoMapConstant.CollectionType.COMMON || mCollectionType == AutoMapConstant.CollectionType.GET_POINT) {
                holder.mResultItemBinding.llActionContainer.setVisibility(View.GONE); //隐藏左滑按钮
            }
        } else {
            if (mCollectionType == AutoMapConstant.CollectionType.COMMON || mCollectionType == AutoMapConstant.CollectionType.GET_POINT) {
                if (mPoiEntities.get(position).getAddress().isEmpty()) {
                    holder.mResultItemBinding.crlPoiDes.setVisibility(View.GONE);
                    holder.mResultItemBinding.crlPoiName.setVisibility(View.VISIBLE);
                }
                //若是常用地址或者收到的点，设置为加图标
                if (mHomeCompanyType == AutoMapConstant.HomeCompanyType.COLLECTION) {
                    if (ConvertUtils.isEmpty(BehaviorPackage.getInstance().isFavorite(mPoiEntities.get(position)))) {
                        holder.mResultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance().
                                getDrawable(R.drawable.img_addq_58));
                        holder.mResultItemBinding.textNavi.setText(R.string.mps_set_add);
                    } else {
                        //已收藏状态显示已添加
                        holder.mResultItemBinding.textNavi.setText(R.string.route_service_list_item_added);
                        holder.mResultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance()
                                .getDrawable(R.drawable.img_route_search_added));
                    }
                } else if (mHomeCompanyType == AutoMapConstant.HomeCompanyType.COMMON) {
                    if (!BehaviorPackage.getInstance().isFrequentAddress(mPoiEntities.get(position))) {
                        holder.mResultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance().
                                getDrawable(R.drawable.img_addq_58));
                        holder.mResultItemBinding.textNavi.setText(R.string.mps_set_add);
                    } else {
                        //已收藏状态显示已添加
                        holder.mResultItemBinding.textNavi.setText(R.string.route_service_list_item_added);
                        holder.mResultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance()
                                .getDrawable(R.drawable.img_route_search_added));
                    }
                } else {
                    //默认显示添加
                    holder.mResultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance().
                            getDrawable(R.drawable.img_addq_58));
                    holder.mResultItemBinding.textNavi.setText(R.string.mps_set_add);
                }
                holder.mResultItemBinding.llActionContainer.setVisibility(View.GONE); //隐藏左滑按钮
            }
        }

        holder.mResultItemBinding.crlPoiDes.setOnClickListener(v -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poi click 详情" + mPoiEntities.get(position).getPid());
            final String input = mPoiEntities.get(position).getPid();
            if (!ConvertUtils.isEmpty(input) && input.contains(".") && !input.startsWith("C")) {
                // 找到第二个小数点的位置
                final int firstDotIndex = input.indexOf('.'); // 第一个小数点的位置
                final int secondDotIndex = input.indexOf('.', firstDotIndex + 1); // 第二个小数点的位置
                if (secondDotIndex != -1 && firstDotIndex != -1) {
                    // 分割字符串
                    final String firstPart = input.substring(0, secondDotIndex - 3); // 第一部分（从开头到第二个小数点前2位）
                    final String secondPart = input.substring(secondDotIndex - 2); // 第二部分（从第二个小数点前2位后开始）

                    // 转换为 double
                    final double firstNumber = Double.parseDouble(firstPart);
                    final double secondNumber = Double.parseDouble(secondPart);
                    final GeoPoint geoPoint = new GeoPoint(firstNumber, secondNumber);
                    mPoiEntities.get(position).setPoint(geoPoint);
                }
            }
            if (mOnItemClickListener != null) {
                mOnItemClickListener.onItemClick(position, mPoiEntities.get(position));
            }
        });

        holder.mResultItemBinding.poiToNavi.setOnClickListener(v -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poi click " + mPoiEntities.get(position).getName()
                    + " ,id: " + mPoiEntities.get(position).getPid()
                    + " ,point: " + mPoiEntities.get(position).getPoint());
            if (mSearchPackage.isAlongWaySearch()) {
                if (RoutePackage.getInstance().isBelongRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, mPoiEntities.get(position))) {
                    return;
                }
            } else {
                //若是常用地址或者收到的点并且已经添加，直接return
                if (mCollectionType == AutoMapConstant.CollectionType.COMMON || mCollectionType == AutoMapConstant.CollectionType.GET_POINT) {
                    if (mHomeCompanyType == AutoMapConstant.HomeCompanyType.COLLECTION) {
                        if (!ConvertUtils.isEmpty(BehaviorPackage.getInstance().isFavorite(mPoiEntities.get(position)))) {
                            return;
                        }
                    } else if (mHomeCompanyType == AutoMapConstant.HomeCompanyType.COMMON) {
                        if (BehaviorPackage.getInstance().isFrequentAddress(mPoiEntities.get(position))) {
                            return;
                        }
                    }
                }
            }
            if (mOnItemClickListener != null) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poi click1 算路/添加常用地址/添加收到的点");
                mOnItemClickListener.onNaviClick(position, mPoiEntities.get(position));
            }
        });

        holder.mResultItemBinding.sllTop.setOnClickListener(v -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poi click 置顶");
            if (mPoiEntities.get(position).getFavoriteInfo().getTop_time() != 0) {
                // 取消置顶
                SettingUpdateObservable.getInstance().onUpdateSyncTime();
                BehaviorPackage.getInstance().topFavorite(mPoiEntities.get(position), false);
//                BehaviorPackage.getInstance().updateFavoriteTopTime(poiEntities.get(position).getFavoriteInfo().getItemId(), 0);
            } else {
                SettingUpdateObservable.getInstance().onUpdateSyncTime();
                BehaviorPackage.getInstance().topFavorite(mPoiEntities.get(position), true);
//                long current = System.currentTimeMillis();
//                BehaviorPackage.getInstance().updateFavoriteTopTime(poiEntities.get(position).getFavoriteInfo().getItemId(), current);
            }
            holder.mResultItemBinding.swipeMenuLayout.smoothClose();
            final List<PoiInfoEntity> poiInfoEntityList = BehaviorPackage.getInstance().getFavoritePoiData();
            // 过滤掉无详细地址的收藏info
            poiInfoEntityList.removeIf(poiInfo -> ConvertUtils.isEmpty(poiInfo.getAddress()));
            ThreadManager.getInstance().postUi(() -> {
                notifyList(poiInfoEntityList);
            });
        });
        holder.mResultItemBinding.sllDelete.setOnClickListener(v -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poi click 删除" + position);
            if (position >= 0 && position < mPoiEntities.size()) {
                final PoiInfoEntity info = mPoiEntities.get(position);
//                BehaviorPackage.getInstance().deleteFavoriteData(info.getFavoriteInfo().getItemId());
                if(ConvertUtils.isNull(info.getOperatorId())){
                    BehaviorPackage.getInstance().removeFavorite(info);
                }else{
                    mOnItemClickListener.updateCollectStatus(mPoiEntities.get(position));
                }
                mPoiEntities.remove(position);
                notifyDataSetChanged();
                // 删除成功后的toast弹框
                ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.sha_deleted));
            }
            if (ConvertUtils.isEmpty(mPoiEntities) && mOnItemClickListener != null) {
                mOnItemClickListener.onListCleared();
            }
            holder.mResultItemBinding.swipeMenuLayout.smoothClose();
        });
    }

    @Override
    public int getItemCount() {
        return mPoiEntities.size();
    }

    public boolean isEmptyData() {
        return mPoiEntities.isEmpty();
    }

    public static class ResultHolder extends RecyclerView.ViewHolder {
        private final CollectResultItemBinding mResultItemBinding;

        public ResultHolder(final CollectResultItemBinding resultItemBinding) {
            super(resultItemBinding.getRoot());
            this.mResultItemBinding = resultItemBinding;
            this.mResultItemBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        /**
         * 点击事件
         * @param position 点击下标
         * @param poiInfoEntity 点击的poi信息
         */
        void onItemClick(int position, PoiInfoEntity poiInfoEntity);

        /**
         * 导航图标点击事件
         * @param position 点击下标
         * @param poiInfoEntity 点击的poi信息
         */
        void onNaviClick(int position, PoiInfoEntity poiInfoEntity);

        /**
         * 列表清空事件 当删除所有列表项之后调用
         */
        void onListCleared();

        void updateCollectStatus(PoiInfoEntity poiInfoEntity);
    }
}