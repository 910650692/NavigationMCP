package com.sgm.navi.hmi.favorite.adapter;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.databinding.ViewDataBinding;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ResourceUtils;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.ItemFavoriteBinding;
import com.sgm.navi.hmi.databinding.ItemFavoriteEmptyBinding;
import com.sgm.navi.hmi.databinding.ItemFavoriteHeaderBinding;
import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.Nullable;

public class FavoriteDataAdapter extends RecyclerView.Adapter<FavoriteDataAdapter.MultiHolder> {

    // 头布局
    public final static int ITEM_VIEW_TYPE_HEADER = 1;

    // 数据为空的布局页面
    public final static int ITEM_VIEW_TYPE_EMPTY = 2;

    private static final String TAG = FavoriteDataAdapter.class.getSimpleName();
    private List<PoiInfoEntity> mFavoriteInfoList;
    private OnItemClickListener mItemClickListener;
    private OnItemOnCreateView mItemOnCreate;
    private ItemFavoriteHeaderBinding bindingHeader;
    private ItemFavoriteEmptyBinding bindingEmpty;
    private int mType = 0;

    public FavoriteDataAdapter() {
        this.mFavoriteInfoList = new ArrayList<>();
    }

    /**
     * setData
     * @param list list
     */
    @SuppressLint("NotifyDataSetChanged")
    public void setData(final List<PoiInfoEntity> list, final int type) {
        this.mType = type;
        final int oldSize = mFavoriteInfoList.size();
        final int newSize = list.size();

        mFavoriteInfoList.clear();
        mFavoriteInfoList.addAll(list);
        if (oldSize == 0 && newSize > 0) {
            notifyItemRemoved(1);
            notifyItemRangeInserted(1, newSize);
        } else if (oldSize > 0 && newSize == 0) {
            notifyItemRangeRemoved(1, oldSize);
            notifyItemInserted(1);
        } else if (oldSize > 0) {
            notifyItemRangeChanged(1, Math.min(oldSize, newSize));
            if (newSize > oldSize) {
                notifyItemRangeInserted(oldSize + 1, newSize - oldSize);
            } else if (newSize < oldSize) {
                notifyItemRangeRemoved(newSize + 1, oldSize - newSize);
            }
        }
    }

    public void setItemClickListener(final OnItemClickListener itemClickListener) {
        this.mItemClickListener = itemClickListener;
    }

    public void setItemHeaderOnCreate(final OnItemOnCreateView itemHeaderOnCreate) {
        this.mItemOnCreate = itemHeaderOnCreate;
    }

    @Nullable
    public ItemFavoriteHeaderBinding getBindingHeader() {
        return bindingHeader;
    }

    @NonNull
    @Override
    public FavoriteDataAdapter.MultiHolder onCreateViewHolder(final @NonNull ViewGroup parent, final int viewType) {
        if (viewType == ITEM_VIEW_TYPE_HEADER) {
            bindingHeader =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                    R.layout.item_favorite_header, parent, false);
            if (mItemOnCreate != null) {
                mItemOnCreate.onCreateHeaderView(bindingHeader);
            }
            return new MultiHolder<>(bindingHeader, viewType);
        } else if (viewType == ITEM_VIEW_TYPE_EMPTY) {
            bindingEmpty =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                    R.layout.item_favorite_empty, parent, false);
            if (mItemOnCreate != null) {
                mItemOnCreate.onCreateEmptyView(bindingEmpty);
            }
            return new MultiHolder<>(bindingEmpty, viewType);
        } else {
            final ItemFavoriteBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                    R.layout.item_favorite, parent, false);
            return new Holder(routeItemBinding, viewType);
        }
    }

    @Override
    public int getItemCount() {
        if (mFavoriteInfoList.isEmpty()) {
            return 2;
        } else {
            return mFavoriteInfoList.size() + 1;
        }
    }

    @Override
    public int getItemViewType(int position) {
        if (position == 0) {
            return ITEM_VIEW_TYPE_HEADER;
        } else if (mFavoriteInfoList.isEmpty() && position == getItemCount() - 1) {
            return ITEM_VIEW_TYPE_EMPTY;
        } else {
            return super.getItemViewType(position);
        }
    }

    @Override
    public void onBindViewHolder(final @NonNull FavoriteDataAdapter.MultiHolder holder, final int position) {
        if (holder.viewType == ITEM_VIEW_TYPE_HEADER) {
            if (mItemOnCreate != null) {
                mItemOnCreate.onResumeHeaderView((ItemFavoriteHeaderBinding) holder.binding);
            }
        } else if (holder.viewType == ITEM_VIEW_TYPE_EMPTY) {
            if (mItemOnCreate != null) {
                mItemOnCreate.onResumeEmptyView((ItemFavoriteEmptyBinding) holder.binding);
            }
        } else {
            final int subPosition = position - 1;
            Holder itemHolder = (Holder) holder;
            itemHolder.binding.setModel(mFavoriteInfoList.get(subPosition));
            itemHolder.binding.swipeMenuLayout.setSwipeEnabled(mType == 0);
            itemHolder.binding.setLayoutPosition(String.valueOf(subPosition + 1));
            if (mFavoriteInfoList.get(subPosition).getFavoriteInfo().getTop_time() != 0) {
                itemHolder.binding.swipeMenuLayout.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.color.setting_bg_top));
                itemHolder.binding.itemFavoriteTopText.setText("取消");
                itemHolder.binding.itemFavoriteTopTag.setVisibility(View.GONE);
                itemHolder.binding.itemFavoriteTopIcon.setImageResource(R.drawable.img_favorite_untop);
            } else {
                itemHolder.binding.swipeMenuLayout.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.color.transparent));
                itemHolder.binding.itemFavoriteTopText.setText("置顶");
                itemHolder.binding.itemFavoriteTopTag.setVisibility(View.GONE);
                itemHolder.binding.itemFavoriteTopIcon.setImageResource(R.drawable.img_favorite_top);
            }
            //查看POI详情
            itemHolder.binding.contentLayout.setOnClickListener(v -> {
                if (mItemClickListener != null) {
                    mItemClickListener.onItemDetailClick(subPosition);
                }
            });

            // 导航到这里
            itemHolder.binding.itemFavoriteNavi.setOnClickListener(v -> {
                if (mItemClickListener != null) {
                    mItemClickListener.onItemNaviClick(subPosition);
                }
            });

            itemHolder.binding.itemFavoriteTop.setOnClickListener(v -> {
                if (mItemClickListener != null) {
                    if (mFavoriteInfoList.get(subPosition).getFavoriteInfo().getTop_time() != 0) {
                        mItemClickListener.onItemCancelTopClick(subPosition);
                    } else {
                        mItemClickListener.onItemTopClick(subPosition);
                    }
                }
                itemHolder.binding.swipeMenuLayout.smoothClose();
            });

            itemHolder.binding.itemFavoriteDelete.setOnClickListener(v -> {
                itemHolder.binding.swipeMenuLayout.quickClose();
                if (mItemClickListener != null) {
                    mItemClickListener.onItemDeleteClick(subPosition);
                }
            });
        }
    }

    public static class Holder extends MultiHolder<ItemFavoriteBinding> {
        public Holder(final ItemFavoriteBinding favoriteBinding, int viewType) {
            super(favoriteBinding, viewType);
            favoriteBinding.setHolder(this);
        }
    }

    public static class MultiHolder<T extends ViewDataBinding> extends RecyclerView.ViewHolder {
        public int viewType;
        public T binding;

        public MultiHolder(final T binding, int viewType) {
            super(binding.getRoot());
            this.binding = binding;
            this.viewType = viewType;
        }
    }

    public interface OnItemClickListener {

        /**
         * onItemDetailClick
         * @param index
         */
        void onItemDetailClick(int index);

        /**
         * onItemNaviClick
         * @param index
         */
        void onItemNaviClick(int index);

        /**
         * onItemTopClick
         * @param index
         */
        void onItemTopClick(int index);

        /**
         * onItemCancelTopClick
         * @param index
         */
        void onItemCancelTopClick(int index);

        /**
         * onItemDeleteClick
         * @param index
         */
        void onItemDeleteClick(int index);
    }

    public interface OnItemOnCreateView {

        /**
         * 头布局 创建监听回调
         *
         * @param itemFavoriteHeaderBinding 布局文件
         */
        void onCreateHeaderView(ItemFavoriteHeaderBinding itemFavoriteHeaderBinding);

        /**
         * 头布局 显示监听回调
         *
         * @param itemFavoriteHeaderBinding 布局文件
         */
        void onResumeHeaderView(ItemFavoriteHeaderBinding itemFavoriteHeaderBinding);

        /**
         * 空白布局 view创建监听回调
         *
         * @param itemFavoriteEmptyBinding 布局文件
         */
        void onCreateEmptyView(ItemFavoriteEmptyBinding itemFavoriteEmptyBinding);

        /**
         * 空白布局 view显示监听回调
         *
         * @param itemFavoriteEmptyBinding 布局文件
         */
        void onResumeEmptyView(ItemFavoriteEmptyBinding itemFavoriteEmptyBinding);
    }
}
