package com.sgm.navi.scene.ui.adapter;

import android.content.res.TypedArray;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.sgm.navi.scene.R;
import com.sgm.navi.scene.api.search.IQuickSearchItemClickListener;
import com.sgm.navi.scene.databinding.QuickSearchItemBinding;

public class QuickSearchListAdapter extends RecyclerView.Adapter<QuickSearchListAdapter.ResultHolder> {
    private String[] mCategories;
    private TypedArray mIcons;
    private IQuickSearchItemClickListener mOnItemClickListener;

    public void setOnItemClickListener(final IQuickSearchItemClickListener listener) {
        mOnItemClickListener = listener;
    }

    /**
     * 设置分类文本和对应图标
     * @param icons 图标列表
     * @param categories 标题文本列表
     */
    public void setCategories(final TypedArray icons, final String[] categories) {
        if (this.mIcons != null) {
            this.mIcons.recycle();
        }
        this.mIcons = icons;
        this.mCategories = categories;
        notifyItemRangeChanged(0, getItemCount());
    }

    @NonNull
    @Override
    public ResultHolder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final QuickSearchItemBinding quickSearchItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.quick_search_item, parent, false);
        return new ResultHolder(quickSearchItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull final ResultHolder holder, final int position) {
        if (mCategories != null && mCategories.length > position) {
            holder.mResultItemBinding.quickName.setText(mCategories[position]);
        }
        if (mIcons != null && mIcons.length() > position) {
            holder.mResultItemBinding.skIvIcon.setImageResource(mIcons.getResourceId(position, 0));
        }

        holder.mResultItemBinding.getRoot().setOnClickListener(v -> {
            if (mOnItemClickListener != null) {
                mOnItemClickListener.onItemClick(position, mCategories[position]);
            }
        });
    }

    @Override
    public int getItemCount() {
        return mCategories != null ? mCategories.length : 0;
    }

    @Override
    public void onDetachedFromRecyclerView(@NonNull final RecyclerView recyclerView) {
        super.onDetachedFromRecyclerView(recyclerView);
        if (mIcons != null) {
            mIcons.recycle();
            mIcons = null;
        }
    }

    public static class ResultHolder extends RecyclerView.ViewHolder {
        private final QuickSearchItemBinding mResultItemBinding;

        public ResultHolder(final QuickSearchItemBinding resultItemBinding) {
            super(resultItemBinding.getRoot());
            this.mResultItemBinding = resultItemBinding;
            this.mResultItemBinding.setHolder(this);
        }
    }
}
